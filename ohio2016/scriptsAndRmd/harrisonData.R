# HARRISON ET AL 2017 PRESENTS CH4 EMISSION RATE DATA FOR SIX RESERVOIRS
# IN THE PACIFIC NORTHWEST.  TABLE S1 OF THE SUPPLEMENTARY INFORMATION
# PRESENTS DIFFUSIVE AND EBULLITIVE FLUX RATES (+/-SE) FOR 'PRE-DRAWDOWN'
# AND 'DURING DRAWDOWN' PERIODS.  THE PRE-DRAWDOWN DATA ARE MOST APPROPRIATE
# FOR OUR ANALYSIS.  DATA WERE ENTERED INTO EXCEL AND ARE READ IN BELOW.

# BRING IN EMISSION RATE DATA---------------
harRate <- read_excel("ohio2016/inputData/literatureData/harrison.xlsx")

names(harRate) = gsub(pattern = c("\\(| |#|)|/|-|\\+"), replacement = ".", 
                     x = names(harRate))

# BRING IN MORPHOMETRIC DATA--------------------
harMorph <- read_excel(paste("ohio2016/inputData/watershedAndMorphology/fromGisUserAprues/",
                             "PNW_Volumes_SAs_053118.xlsx", sep = ""))

names(harMorph) = gsub(pattern = c("\\(| |#|)|/|-|<|\\+"), replacement = ".", 
                       x = names(harMorph)) %>%
  tolower()

# Adopt consistent naming conventions
harMorph <- mutate(harMorph,
                   proportion.of.reservoir.area.shallower.than.xm.contour =
                     percent.reservoir.area.shallower.than.xm.contour / 100) %>%
  rename(Lake_Name = lake,
         surface.area.less.3m.or.closest = 
           surface.area...3m..or.closest.contour.) %>% 
  mutate(Lake_Name = replace(Lake_Name,
                             grep(pattern = "Elum", x = Lake_Name),
                             "Cle Elum"),
         Lake_Name = replace(Lake_Name,
                             grep(pattern = "Foster", x = Lake_Name),
                             "Foster"),
         Lake_Name = replace(Lake_Name,
                             grep(pattern = "Boyle", x = Lake_Name),
                             "J.C. Boyle"),
         Lake_Name = replace(Lake_Name,
                             grep(pattern = "Kachess", x = Lake_Name),
                             "Kachess"),
         Lake_Name = replace(Lake_Name,
                             grep(pattern = "Keno", x = Lake_Name),
                             "Keno"),
         Lake_Name = replace(Lake_Name,
                             grep(pattern = "Lacamas", x = Lake_Name),
                             "Lacamas"))

# Poor bathymetry coverage for Keno.  Morpho calcs are not reliable. Omit.
harMorph <- filter(harMorph, Lake_Name != "Keno")




# BRING IN LU AND MORPHOLOGY--------------------
harMorphLu <- read_excel(paste("ohio2016/inputData/watershedAndMorphology/fromGisUserAprues/",
                               "ORWA_GHG_Emissions_053118.xlsx", sep =""))

names(harMorphLu) = gsub(pattern = c("\\(| |#|)|/|-|_|\\+"), replacement = ".", 
                         x = names(harMorphLu)) %>%
  tolower()

harMorphLu <- harMorphLu %>% select(-resevoir.area.m2) # Better data in harMorph

# Adopt consistent naming convention
harMorphLu <- harMorphLu %>% 
  rename(Lake_Name = lake.name) %>%
  mutate(Lake_Name = replace(Lake_Name,
                             grep(pattern = "Elum", x = Lake_Name),
                             "Cle Elum"),
         Lake_Name = replace(Lake_Name,
                             grep(pattern = "Foster", x = Lake_Name),
                             "Foster"),
         Lake_Name = replace(Lake_Name,
                             grep(pattern = "Boyle", x = Lake_Name),
                             "J.C. Boyle"),
         Lake_Name = replace(Lake_Name,
                             grep(pattern = "Kachess", x = Lake_Name),
                             "Kachess"),
         Lake_Name = replace(Lake_Name,
                             grep(pattern = "Keno", x = Lake_Name),
                             "Keno"),
         Lake_Name = replace(Lake_Name,
                             grep(pattern = "Lacamas", x = Lake_Name),
                             "Lacamas"))


# MERGE MORPHOLOGY AND 'MORPHOLOGY + LU'.  UPDATE PROP < 3M-----------------

# harMorphLu contains data from two reservoirs not included in
# Harrison et al 2017.  Omit via left_join
harMerge <- left_join(harMorph, harMorphLu)

# Need estimate of surface area < 3m depth for lakes where shallowest
# bathymetry was > 3m.  For these systems, we calculated the area < the
# minimum contour line.  We can estimate area <3m if we assume constant bed slope
# from shore to minimum contour line.  It is implimented as follows:
# 1) calculate mean distance from shore to minimum contour line.  Calculated
#    as area < min contour line / perimeter.
# 2) calculate bed slope as 'rise/run', where rise is contour depth and run
#    is distance from shore.
# 3) calculate distance to 3m depth using bed slope.
harMerge <- harMerge %>%
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

# Calculate Derived Quantities
harMerge <- mutate(harMerge, 
                  rda = watershed.area.m2 / surface.area..m2.,
                  si = res.perimeter.m / (2*sqrt(pi*surface.area..m2.)),
                  percent.agg.ag = percent.pasture.hay + 
                    percent.cultivated.crops)
# FINAL MERGES AND FORMATTING-----------------------
# Merge watershed/morphology dataframe with emission rate dataframe.
# harRate contains emission rate data for Keno, but the bathymetry data
# for that site are bad and where filtered out of harMerge.  Use left_join
# to strip keno from merge.
harDat <- left_join(harMerge, harRate)



# Format for consistency with meanVariance.c.lake.lu.agg
harDat <- harDat %>%
  rename(reservoir.area.m2.morpho = surface.area..m2.,
         mean.depth.m.morpho = mean.reservoir.depth..m.,
         max.depth.ft = max.reservoir.depth..m.,
         reservoir.volume.m3 = volume.m3.) %>%        #ATTEND TO UNIT CONVERSION
  select(-percent.perennial.ice.snow,  # omit unneeded columns
         -largeLitWidth, -litSlope, -estLitWidth, -estLitArea,
         -surface.area.less.3m.or.closest, -xm.shallow.contour..m.,
         -notes, -more.notes, -source, -state, - reservoir.volume.m3,
         -proportion.of.reservoir.area.shallower.than.xm.contour,
         -percent.reservoir.area.shallower.than.xm.contour,
         -reservoir.id, -dam, -dam.former.name, -main.dam, 
         -reservoir.polygon.yn, -issue.type, -overlap.watershed.yn  
  ) %>%
  mutate(Subpopulation = "Lake")


# # Merge with meanVariance.c.lake.lu
# meanVariance.c.lake.lu <- bind_rows(harDat, meanVariance.c.lake.lu)





