## THIS SCRIPT WAS ORIGINALLY INCLUDED IN 'OhioSurveyDesign.Rmd' AND PRODUCED
## AN OBJECT CALLED survRes WHICH WAS USED FOR THE ORIGINAL SURVEY DESIGN.  
## HERE I MODIFY THE SCRIPT TO INCLUDE ADDITIONAL MORPHOLOGY DATA (i.e. mean depth)
## CALCULATED FOR THE FINAL 32 RESERVOIRS INCLUDED IN THE SURVEY.  I ALSO CHANGED 
## THE OBJECT NAME TO descRes TO AVOID DUPLICATION WITH survRes WHICH IS STILL
## CREATED IN OhioSurveyDesign.Rmd.

#Read, Inspect, and Format Ohio NID Data
  
ohioRes <- read_excel("ohio2016/inputData/ohioReservoirGeomorphology12.17.15.xlsx") 
names(ohioRes) = tolower(names(ohioRes)) # Change all names to all lowercase letters
names(ohioRes) = gsub(pattern = "/", replacement = "_", x = names(ohioRes)) # / causing problems with select
ohioRes  <-  mutate(ohioRes, 
                    lake_name = tolower(lake_name), # lowercase place names
                    dam = tolower(dam),  # lowercase place names
                    dam_former_name = tolower(dam_former_name)) %>% # lowercase place names
  rename(percent_cultivated_crops = percent_culitvated_crops) %>% # correct missspelling
  mutate(percent_openwater = percent_openwater * 100, # labeled 'percent', but reported as proportion
         percent_urban = percent_urban * 100, # labeled 'percent', but reported as proportion
         percent_forest = percent_forest * 100, # labeled 'percent', but reported as proportion
         percent_pasture_hay = percent_pasture_hay * 100, # labeled 'percent', but reported as proportion
         percent_cultivated_crops = percent_cultivated_crops * 100,  # labeled 'percent', actually proportion
         percent_wetlands = percent_wetlands * 100) %>% # labeled 'percent', actually proportion
  select(-contour_depth_link) # remove metadata
str(ohioRes)



#This next bit of code estimates max reservoir depth from the reported deepest contour lines
ohioRes <- mutate(ohioRes,
                  min_contour_ft = as.numeric(gsub("-.*$", "", max_contour_ft)),  # extract prior to "-"
                  max_contour_ft = as.numeric(gsub(".*-", "", max_contour_ft)), # extract after"-"
                  max_depth_ft = ifelse(is.na(max_depth_ft),
                                        (min_contour_ft + (contour_interval_ft/2)),
                                        max_depth_ft))



# Data set contains some reservoirs not appropriate for this analysis.  
# Pull out non-main dam, industrial reservoirs, and other troublesome reservoirs.  
# This narrows the pool to 57 reservoirs.

ohioResC <- filter(ohioRes, !grepl("Riverine|Not main dam.|Offstream|Connected|Industrial", issue_type))
length(unique(ohioResC$lake_name))  # 57 reservoirs


# Replace "_" in column names with ".".  Done to be consistent with files read in later.

names(ohioResC) = gsub(pattern = "_", replacement = ".", x = names(ohioResC))  # replace "_" with "." in names


# Read, Inspect, and Format USACE LD Reservoir Data
# Four of USACE LD reservoirs are located in Ohio and are
# included in the NID data set.  Will strip these out to avoid duplicates.

# Reservoir geometry
ldGeom <- read.xls("ohio2016/inputData/WA 3-93 reservoirsurfacegeometry.xlsx", 
                   sheet="Task 1a", stringsAsFactors = FALSE)
ldGeom <- ldGeom[1:20, c("Reservoir", "NHDArea.sqkm.", "NHDPerimeter.mi.", "Fetch..m.")]  # Subset
names(ldGeom) <- c("Reservoir", "Reservoir.Area.sqkm", "Reservoir.Perimeter.km", "Fetch.m.")  # Rename perimeter to km
ldGeom$Reservoir.Perimeter.km <- ldGeom$Reservoir.Perimeter.km * 1.609  # Converts mile to kilometer

# Watershed land use
ldLu <- read.xls("ohio2016/inputData/WatershedAreaCharacterization.xlsx", 
                 sheet="FinalForm", stringsAsFactors = FALSE)
str(ldLu)
ldLu <- subset(ldLu, select=-c(Code, State, X))
names(ldLu) = c("Reservoir", "Watershed.area.km2", "prop.crops", "prop.pasture", "prop.range", "prop.water",
                "prop.wetland", "prop.urban", "prop.forest")

# Merge reservoir geometry and land use data
ldRes <- merge_recurse(dfs = list(ldGeom, ldLu))
names(ldRes)

# Strip out four reservoirs in Ohio. These data are already included in ohioResC.  
# Omit harsha (EFR), Caesar(CCK), CJ Brown (CBR), and West Fork (WFR)
ldRes <- filter(ldRes, !(Reservoir %in% c("EFR", "CCK", "CBR", "WFR")))

# Format the Louisville District reservoir data to be consistent with ohioRes.  
# This is necessary to facilitate merging the two data sources.

names(ldRes) = tolower(names(ldRes))
ldRes <- mutate(ldRes, reservoir.area.m2 = reservoir.area.sqkm * 1000000,
                res.perimeter.m = reservoir.perimeter.km * 1000,
                watershed.area.m2 = watershed.area.km2 * 1000000) %>%
  rename(res.fetch.m = fetch.m.,
         lake.name = reservoir,
         percent.openwater = prop.water, # labeled proportion, actually percent
         percent.urban = prop.urban,  # labeled proportion, actually percent
         percent.forest = prop.forest,  # labeled proportion, actually percent
         percent.pasture.hay = prop.pasture,  # labeled proportion, actually percent
         percent.cultivated.crops = prop.crops,  # labeled proportion, actually percent
         percent.wetlands = prop.wetland) %>%  # labeled proportion, actually percent
  select(lake.name, res.fetch.m, reservoir.area.m2, res.perimeter.m, watershed.area.m2, percent.openwater,
         percent.urban, percent.forest, percent.pasture.hay, percent.cultivated.crops,
         percent.wetlands, res.fetch.m, lake.name)


#Merge ldRes and ohioRes Dataframes

descRes <- merge(ldRes, ohioResC, 
                 by = c("lake.name", "res.perimeter.m", "res.fetch.m", "reservoir.area.m2",
                        "watershed.area.m2", "percent.openwater", "percent.urban", 
                        "percent.forest", "percent.pasture.hay", "percent.cultivated.crops",
                        "percent.wetlands", "lake.name"),
                 all = TRUE)

descRes$lLake_Name <- ifelse(descRes$lake.name == "BVR",
                            "brookville lake",
                            ifelse(descRes$lake.name == "BHR",
                                   "buckhorn lake",
                                   ifelse(descRes$lake.name == "ceaser creek lake",
                                          "caesar creek lake",
                                          ifelse(descRes$lake.name == "CFK",
                                                 "carr fork lake",
                                                 ifelse(descRes$lake.name == "CRR",
                                                        "cave run lake",
                                                        descRes$lake.name)))))

# Filter out sites not included in 2016 survey work
# translationKeydf is made in masterLibrary.R
descRes <- filter(descRes, lLake_Name %in% tolower(translationKeydf$Lake_Name)) %>%
  select(-lake.name) # no longer need this column


###############################################################################
# Data imported above are similar to those used for the survey design 
# (2016OhioSurveyDesign.Rmd).  Now we need to pull in the morphology and hydrology 
# data Michelle Platz compiled.  Merging these two data sources is a bit confusing
# because Michelle had to calculate reservoir area in the process of calculating lake 
# volume and mean depth.  This means that 'reservoir area' will represented twice
# in the data set.  The first estimate was used for the survey design while the
# second was used for Michelle's morphology calculations.  

# Reservoir Hydrology and morphology
morphoHydro <- read.xls(xls = file.path("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/",
                                  "multiResSurvey2016/data/lakeMorpho/",
                                  "reservoirVolumeAndMeanDepthCalculations_Army.xlsx",
                                  fsep = ""), 
                  sheet="done", stringsAsFactors=FALSE, skip = 1)

names(morphoHydro) = tolower(names(morphoHydro))

#keep Lake_Name uppercase to be consistent with eqAreaData and meanVariance.c
morphoHydro <- rename(morphoHydro, Lake_Name = lake_name) 

# Simplify df and create lLower_Lake for merging with above
morphoHydro <- select(morphoHydro, Lake_Name, outflow.cfs, reservoir.volume.m3,
                      reservoir.area.m2.morpho, mean.depth.m.morpho,
                      anoxicfrac) %>%
  mutate(lLake_Name = tolower(Lake_Name))


# Merge
descRes <- merge(descRes,morphoHydro, all = TRUE)

str(descRes)  #32 obs, good

# The only remaining data to bring in is the max depth estimate for the LD
# reservoirs not in Ohio.  In 2016OhioSurveyDesign.Rmd these data were brought 
# from a spreadsheet not used here.  I will just hardcode them in here.
descRes[descRes$Lake_Name == "Brookville Lake", "max.depth.ft"] = 110
descRes[descRes$Lake_Name == "Buckhorn Lake", "max.depth.ft"] = 50
descRes[descRes$Lake_Name == "Carr Fork Lake", "max.depth.ft"] = 66
descRes[descRes$Lake_Name == "Cave Run Lake", "max.depth.ft"] = 70


# Calculate Derived Quantities
descRes <- mutate(descRes, 
                  residence.time.yr = (reservoir.volume.m3 * 35.3147)  # convert to ft3
                                / (outflow.cfs*60*60*24*365),
                  rda = watershed.area.m2 / reservoir.area.m2,
                  si = res.perimeter.m / (2*sqrt(pi*reservoir.area.m2)),
                  percent.agg.ag = percent.pasture.hay + 
                    percent.cultivated.crops)



