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
bevCove <- data.frame(Reservoir = c("Allatoona", "Douglas", "Fontana",
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

# Aggregate and scale ebullition to whole lake
bevEbul <- mutate(bevEbul, 
                  # Convert from per day to per hour
                  ch4.erate.mg.h_Estimate = Ebullition.CH4..mg.m2.day. / 24,
                  co2.erate.mg.h_Estimate = Ebullition.CO2..mg.m2.day. / 24) %>%
  select(Reservoir, co2.erate.mg.h_Estimate, ch4.erate.mg.h_Estimate) %>%
  group_by(Reservoir) %>%
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
  # Assume equal to mean of all other reservoirs.  Wil, feel free to 
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
         ch4.erate.mg.h_Estimate = ch4.erate.mg.h_Estimate * propCove)


# DIFFUSION DATA--------------  
# Read in diffusive emission rates.  Measurements made in open-water and cove
# areas.
bevDif <- read_excel("ohio2016/inputData/literatureData/Bevelhimer synoptic survey figs Feb2015_amf.xlsx",
                      sheet = "dome flux")

names(bevDif) = gsub(pattern = c("\\(| |#|)|/|-|\\+"), replacement = ".", 
                      x = names(bevDif))

# Aggregate and scale diffusion to whole lake
bevDif <- mutate(bevDif, 
                 # Convert from per day to per hour
                 ch4.drate.mg.m2.h_Estimate = CHAMBER.CH4.Flux..mg.m2.d. / 24,
                 co2.drate.mg.m2.h_Estimate = CHAMBER.CO2.Flux..mg.m2.d. / 24) %>%
  rename(habitat = X__2) %>% # habitat is cove or 'open-water'
  select(Reservoir, habitat, ch4.drate.mg.m2.h_Estimate, co2.drate.mg.m2.h_Estimate) %>%
  group_by(Reservoir, habitat) %>%
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
  # Still need to deal with std error
  group_by(Reservoir) %>% 
  # Lake-scale estimate is sum of habitat weighted estimates for each lake
  summarize(co2.drate.mg.m2.h_Estimate = sum(co2.drate.mg.m2.h_Estimate.weighted),
            ch4.drate.mg.m2.h_Estimate = sum(ch4.drate.mg.m2.h_Estimate.weighted))

         
# LAND USE AND MORPHOLOGY DATA-------------------------------
# NOT READY YET (4/27/18)  

# Pegasus will not be providing lake volume or mean depth due to the excessive
# work required to digitize the bathymetry contours.  Will use the data from 
# Table 1 of Bevelhimer et al for these values.
bevTable1 <- data.frame(Reservoir = c("Allatoona", "Douglas", "Fontana",
                                      "Guntersville", "Hartwell", "Watts Bar"),
                        reservoir.volume.m3 = c(453, 1334, 1780, 1256, 3145, 1396) *
                          10^6,
                        surface.area.km2 = c(49, 115, 43, 279, 226, 176)) %>%
  mutate(mean.depth.m.morpho = reservoir.volume.m3 / 
           (surface.area.km2 * 1000000)) %>%
  select(-surface.area.km2)  # will use SQ from Pegasus


# Don't forget about derived quantities
mutate(df, 
                   rda = watershed.area.m2 / surface.area..m2.,
                   si = res.perimeter.m / (2*sqrt(pi*surface.area..m2.)),
                   percent.agg.ag = percent.pasture.hay + 
                     percent.cultivated.crops)

# FINAL MERGES-------------------
# At the end of all this, the Bevelhimer data will get merged with 
# meanvariance.c.lake.lu
########################################################################-
########################################################################-
#Change Reservoir to Lake_Name at end.
