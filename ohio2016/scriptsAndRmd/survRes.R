## THIS SCRIPT WAS ORIGINALLY INCLUDED IN 'OhioSurveyDesign.Rmd' WHICH WAS
## USED FOR THE ORIGINAL SURVEY DESIGN.  THE survRes OBJECT CREATED IN THIS 
## SCRIPT IS USED IN exploratoryPlots.R.  RUNNING SCRIPT HERE TO MAKE SURE 
## OBJECT GETS PUSHED INTO WORKSPACE.

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

# Reservoir Hydrology
ldHyd <- read.xls("ohio2016/inputData/LouisDistReservoirHydrology.xlsx", 
                  sheet="hydrology", stringsAsFactors=FALSE)
ldHyd <- select(ldHyd, -Lake)

# Merge reservoir geometry, land use, and hydrology data
ldRes <- merge_recurse(dfs = list(ldGeom, ldLu, ldHyd))
names(ldRes)

# Define residence time
ldRes$residence.time.yr <- with(ldRes, (summer.storage.acre.ft*43560)  # convert to ft3
                                / (outflow.cfs*60*60*24*365))  # convert ot ft3 per year
# Define mean depth
ldRes$mean.depth.ft <- with(ldRes, (summer.storage.acre.ft*43560) /  # convert to ft3
                              (Reservoir.Area.sqkm * (10763.91/0.001)))  # convert to ft2
# Define Relative Drainage Area (RDA)
ldRes$rda <- with(ldRes, (Watershed.area.km2/Reservoir.Area.sqkm))

# Strip out four reservoirs in Ohio
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
         percent.wetlands, res.fetch.m, lake.name, summer.storage.acre.ft, outflow.cfs,
         max.depth.ft, residence.time.yr, mean.depth.ft, rda)


#Merge ldRes and ohioRes Dataframes

survRes <- merge(ldRes, ohioResC, 
                 by = c("lake.name", "res.perimeter.m", "res.fetch.m", "reservoir.area.m2",
                        "watershed.area.m2", "percent.openwater", "percent.urban", 
                        "percent.forest", "percent.pasture.hay", "percent.cultivated.crops",
                        "percent.wetlands", "max.depth.ft", "lake.name"),
                 all = TRUE)