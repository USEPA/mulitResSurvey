# BEVELIMER ET AL 2016 PRESENTS CH4 AND CO2 EMISSIONS RATES FOR SIX
# LARGE RESERVOIRS IN THE SOUTHEASTERN UNITED STATES.  NO MEASUREMENTS OF
# VARIANCE (i.e. STD ERROR, STD DEV, ...) ARE PRESENTED IN PAPER, SO RAW
# DATA WERE OBTAINED FROM CORRESPONDING AUTHOR.  THE CODE BELOW READS AND
# AGGREGRATES THE DATA.  NEPTUNE WILL ADD CODE TO PROPERLY HANDLE THE VARIANCE
# ESTIMATES.

# Bevelhimer, M. S., A. J. Stewart, A. M. Fortner, J. R. Phillips and J. J.
# Mosher (2016). "CO2 is Dominant Greenhouse Gas Emitted from Six Hydropower 
# Reservoirs in Southeastern United States during Peak Summer Emissions." 
# Water 8(1): 14.

# HABITAT DATA---------------
# Each reservoir was stratified into 'cove' and 'open-water' areas. Proportion
# of reservoir surface area that is 'cove' is reported in Table 1 of paper
# and hard coded into dataframe below.
bevCove <- data.frame(Lake_Name = c("Allatoona", "Douglas", "Fontana",
                                    "Guntersville", "Hartwell", "Watts Bar"),
                      propCove = c(0.63, 0.41, 0.44, 
                                   0.49, 0.48, 0.46),
                      stringsAsFactors = FALSE)

# EBULLITION DATA-------------------
# Read in ebullition (bubbling) rates.  Measurements only made in coves.  Author
# assumes an ebullition rate of zero in open-water areas.
bevEbul <- read_excel("ohio2016/inputData/literatureData/Bevelhimer synoptic survey figs Feb2015_amf.xlsx",
                      sheet = "ebullition flux")

names(bevEbul) = gsub(pattern = c("\\(| |#|)|/|-|\\+"), replacement = ".", 
                     x = names(bevEbul))
bevEbul <- rename(bevEbul, Lake_Name = Reservoir)

# Aggregate and scale ebullition to whole lake
bevEbul <- mutate(bevEbul, 
                  # Convert from per day to per hour
                  ch4.erate.mg.h_Estimate = Ebullition.CH4..mg.m2.day. / 24,
                  co2.erate.mg.h_Estimate = Ebullition.CO2..mg.m2.day. / 24) %>%
  select(Lake_Name, co2.erate.mg.h_Estimate, ch4.erate.mg.h_Estimate) %>%
  group_by(Lake_Name) %>%
  summarize(
    # calculating SE error here to be consistent with output for grts function.
    # evalGBM.R and runGBM.R weight by 1/SE^2
    co2.erate.mg.h_StdError = sd(co2.erate.mg.h_Estimate) / # std error
      sqrt(length(co2.erate.mg.h_Estimate)),
    ch4.erate.mg.h_StdError = sd(ch4.erate.mg.h_Estimate) / # std error
      sqrt(length(ch4.erate.mg.h_Estimate)),
    co2.erate.mg.h_Estimate = mean(co2.erate.mg.h_Estimate), # mean
    ch4.erate.mg.h_Estimate = mean(ch4.erate.mg.h_Estimate)  # mean
  ) %>%
  # Hartwell only has one measurement, therefore can't calculate SE
  # Assume equal to mean of all other reservoirs.  Will, feel free to 
  # modify this approach.
  mutate(co2.erate.mg.h_StdError = replace(co2.erate.mg.h_StdError,
                                           which(is.nan(co2.erate.mg.h_StdError)),
                                           mean(co2.erate.mg.h_StdError, na.rm = TRUE)),
         ch4.erate.mg.h_StdError = replace(ch4.erate.mg.h_StdError,
                                           which(is.nan(ch4.erate.mg.h_StdError)),
                                           mean(ch4.erate.mg.h_StdError, na.rm = TRUE))) %>%
  # Scale to entire lake.  Still need to convert StdError to account for scaling
  full_join(bevCove) %>% # bring in proportion of cove
  mutate(co2.erate.mg.h_Estimate = co2.erate.mg.h_Estimate * propCove,
         ch4.erate.mg.h_Estimate = ch4.erate.mg.h_Estimate * propCove) %>%
  select(-propCove)


# DIFFUSION DATA--------------  
# Read in diffusive emission rates.  Measurements made in open-water and cove
# areas.
bevDif <- read_excel("ohio2016/inputData/literatureData/Bevelhimer synoptic survey figs Feb2015_amf.xlsx",
                      sheet = "dome flux")

names(bevDif) = gsub(pattern = c("\\(| |#|)|/|-|\\+"), replacement = ".", 
                      x = names(bevDif))
bevDif <- rename(bevDif, Lake_Name = Reservoir)

# Aggregate and scale diffusion to whole lake
bevDif <- mutate(bevDif, 
                 # Convert from per day to per hour
                 ch4.drate.mg.m2.h_Estimate = CHAMBER.CH4.Flux..mg.m2.d. / 24,
                 co2.drate.mg.m2.h_Estimate = CHAMBER.CO2.Flux..mg.m2.d. / 24) %>%
  rename(habitat = X__2) %>% # habitat is cove or 'open-water'
  select(Lake_Name, habitat, ch4.drate.mg.m2.h_Estimate, co2.drate.mg.m2.h_Estimate) %>%
  group_by(Lake_Name, habitat) %>%
  summarize(
    co2.drate.mg.m2.h_StdError = sd(co2.drate.mg.m2.h_Estimate) / # std error
      sqrt(length(co2.drate.mg.m2.h_Estimate)),
    ch4.drate.mg.m2.h_StdError = sd(ch4.drate.mg.m2.h_Estimate) / # std error
      sqrt(length(ch4.drate.mg.m2.h_Estimate)),
    co2.drate.mg.m2.h_Estimate = mean(co2.drate.mg.m2.h_Estimate), # mean
    ch4.drate.mg.m2.h_Estimate = mean(ch4.drate.mg.m2.h_Estimate)  # mean
  ) %>%
  # Weight habitat specific (cove, open-water) emission rates by proportion of
  # reservoir surface occupied by each habitat.
  # Wil please address std error calcs
  full_join(bevCove) %>% # Add proportion of cove
  mutate(co2.drate.mg.m2.h_Estimate.weighted = ifelse(habitat == "cove",
                                                      co2.drate.mg.m2.h_Estimate * propCove,
                                                      co2.drate.mg.m2.h_Estimate * (1 - propCove)),
         ch4.drate.mg.m2.h_Estimate.weighted = ifelse(habitat == "cove",
                                                      ch4.drate.mg.m2.h_Estimate * propCove,
                                                      ch4.drate.mg.m2.h_Estimate * (1 - propCove))) %>%
  # Now scale habitat weighted emission rates to entire lake.  
  # Still need to deal with std error!!!!!!!!!!!!!!!!!
  group_by(Lake_Name) %>% 
  # Lake-scale estimate is sum of habitat weighted estimates for each lake
  summarize(co2.drate.mg.m2.h_Estimate = sum(co2.drate.mg.m2.h_Estimate.weighted),
            ch4.drate.mg.m2.h_Estimate = sum(ch4.drate.mg.m2.h_Estimate.weighted))

         
# TOTAL EMISSIONS DATA--------------------
# Total emissions is simply the sum of diffusive and ebullitive
# Need to deal with SE calculations
bevAllEmis <- merge(bevEbul, bevDif) %>%
  mutate(ch4.trate.mg.h_Estimate = ch4.drate.mg.m2.h_Estimate +
           ch4.erate.mg.h_Estimate,
         co2.trate.mg.h_Estimate = co2.erate.mg.h_Estimate +
           co2.drate.mg.m2.h_Estimate)

# LAND USE AND MORPHOLOGY DATA-------------------------------
# Pegasus will not be providing lake volume or mean depth due to the excessive
# work required to digitize the bathymetry contours.  Will use the data from 
# Table 1 of Bevelhimer et al for these values.
bevTable1 <- data.frame(Lake_Name = c("Allatoona", "Douglas", "Fontana",
                                      "Guntersville", "Hartwell", "Watts Bar"),
                        reservoir.volume.m3 = c(453, 1334, 1780, 1256, 3145, 1396) *
                          10^6,
                        surface.area.km2 = c(49, 115, 43, 279, 226, 176)) %>%
  mutate(mean.depth.m.morpho = reservoir.volume.m3 / 
           (surface.area.km2 * 1000000)) %>%
  select(-surface.area.km2)  # will use SA from Pegasus

# Read in Pegasus data
bevMorph <- read_excel(paste("ohio2016/inputData/watershedAndMorphology/fromGisUserAprues/",
                             "Bevelhimer_ReservoirDescriptors053118.xlsx", sep = ""),
                       sheet = "Variables")

names(bevMorph) = gsub(pattern = c("\\(| |#|)|/|-|_|<|\\+"), replacement = ".", 
                       x = names(bevMorph)) %>%
  tolower()

# Adopt consistent naming conventions
bevMorph <- mutate(bevMorph,
                   proportion.of.reservoir.area.shallower.than.xm.contour =
                     percent.reservoir.area.shallower.than.xm.contour / 100) %>%
  rename( Lake_Name = lake,
          watershed.area.m2 = watershed.area..m2.,
          res.perimeter.m = perimeter..m.,
          res.fetch.m = fetch..m.,
          surface.area.less.3m.or.closest = surface.area...3m..or.closest.contour.) 

# Need estimate of surface area < 3m depth for lakes where shallowest
# bathymetry was > 3m.  For these systems, we calculated the area < the
# minimum contour line.  We can estimate area <3m if we assume constant bed slope
# from shore to minimum contour line.  It is implimented as follows:
# 1) calculate mean distance from shore to minimum contour line.  Calculated
#    as area < min contour line / perimeter.
# 2) calculate bed slope as 'rise/run', where rise is contour depth and run
#    is distance from shore.
# 3) calculate distance to 3m depth using bed slope.
bevMorph <- bevMorph %>%
  mutate( # first calculate mean distance from shore to contour
    largeLitWidth = ifelse(xm.shallow.contour..m. != 3, # If min contour NOT 3m!
                           surface.area.less.3m.or.closest /
                             res.perimeter.m,
                           NA), # else NA
    litSlope = xm.shallow.contour..m. / largeLitWidth, # rise/run out to contour mark
    estLitWidth = 3 / litSlope,  # Distance to 3m contour line
    estLitArea = estLitWidth * res.perimeter.m, # Area of res < 3m
    prop.less.3m = ifelse(xm.shallow.contour..m. != 3,
                          estLitArea / surface.area..m2., # use estimated value
                          proportion.of.reservoir.area.shallower.than.xm.contour) # use value from bathymetry
  ) 


# Calculate derived quantities
bevMorph <- mutate(bevMorph, 
                   rda = watershed.area.m2 / surface.area..m2.,
                   si = res.perimeter.m / (2*sqrt(pi*surface.area..m2.)),
                   percent.agg.ag = percent.pasture.hay + 
                     percent.cultivated.crops)



# FINAL MERGES-------------------
bevMerge <- Reduce(function(...) merge(..., all=T), 
                   list(bevAllEmis, bevTable1, bevMorph))

# Format for consistency with meanVariance.c.lake.lu
bevMerge <- bevMerge %>%
  rename(reservoir.area.m2.morpho = surface.area..m2.) %>%
  mutate(Subpopulation = "Lake",
         citation = "Bevelhimer",
         max.depth.ft = max.reservoir.depth..m.*3.28) %>% # convert m to ft
  select(-percent.perennial.ice.snow,  # omit unneeded columns
         -largeLitWidth, -litSlope, -estLitWidth, -estLitArea,
         -surface.area.less.3m.or.closest, -xm.shallow.contour..m.,
         -notes, -source, 
         -proportion.of.reservoir.area.shallower.than.xm.contour,
         -percent.reservoir.area.shallower.than.xm.contour,
         -reservoir.volume.m3, -state, -max.reservoir.depth..m.) 

# # Merge with meanVariance.c.lake.lu
ncol(meanVariance.c.lake.lu); nrow(meanVariance.c.lake.lu) # 120 columns, 40 rows

meanVariance.c.lake.lu <- bind_rows(bevMerge, meanVariance.c.lake.lu) %>%
  as.data.frame() # convert back to df.  tibbleDf causes problems.

ncol(meanVariance.c.lake.lu); nrow(meanVariance.c.lake.lu) # 120 columns, 46 rows, good
