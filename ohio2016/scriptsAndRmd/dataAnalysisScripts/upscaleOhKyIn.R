# UPSCALE OHIO KY IN-----------------------
# The NHD data read in below doesn't contain shoreline information from lakeMorpho.
# That data is available in the lakeMorpho .gdb, however:
# "M:\GIS_data\lakeMorpho\LakeMorphGdb.gdb"
# Since we are not using shoreline information as a predictor variable,
# I'm not going to bother reading it in here, but we could.


# STEP 1: RESERVOIRS IN OHIO/KENTUCKY/INDIANA---------------
# Amy and Ellen extracted all NHD waterbodies coded as 
# Amy: "I selected out al lFTYPE == “LakePond” | “Reservoir  in the KY, OH, IN  
# from NHD Plus V2 Waterbody layers MS_08, MS_07, MS_06, MS_05, and GL_04 
# (removing the two great lakes).  I added a column called NaturalLakes_State.  
# If a state indicated the lake was natural I put the name of the state.  
# Added column called NLAOrigin.  Three options – NATURAL, MAN-MADE, or N/A.
# N/A – means that it was not in the NLA studies.

# More details here:
# https://usepa-my.sharepoint.com/:o:/r/personal/damico_ellen_epa_gov/Documents/
# Notebooks/Technical%20Directive_%20GIS%20Support%20for%20Reservoir%20Project
# ?d=w3792ac4085264b11afd63818b1f2df23&csf=1&e=QtzNvg

# Permanent data location here:
# https://usepa-my.sharepoint.com/:u:/r/personal/damico_ellen_epa_gov/Documents/
# Deliverables/TD-GIS%20Support%20for%20Reservoir%20Project/
# LakeReservoirs.gdbv2.zip?csf=1&e=eXYTTW

# geodatabase extracted to shapefile and .dbf read here
ohKyInWb <- foreign::read.dbf(file = paste0("M:/GIS_data/NHDPlusV2/",
                                            "OH_KY_IN_waterbody/",
                                            "NHDPlus_Lakes_Reservoirs_KY_IN_OH.dbf"),
                              as.is = TRUE, )
nrow(ohKyInWb) # 12,000 waterbodies
table(ohKyInWb$FTYPE) # 11880 lake/pond, only 120 NHD coded reservoirs!

# Filter out natural lakes identifed by Amy and Ellen (see above)
ohKyInRes <- filter(ohKyInWb, is.na(NaturalLak), NLAOrigin != "Natural")
nrow(ohKyInRes) # 11885, therefore 115 natural lakes removed.  All others reservoirs.

# Remove variables not needed or to be replaced by lakeMorpho data
ohKyInRes <- ohKyInRes %>% select(-AREASQKM, -SHAPE_LENG, -Shape_Le_1, 
                                  -Shape_Area, -NLAOrigin, -NaturalLak)

# STEP 2:  lCat DATA FOR RESERVOIRS IN OHIO/KENTUCKY/INDIANA----------------
# Read in needed watershed files from lakeCat
# List of .CSV files containing the lakeCat data used in model.  Not all files needed.
lcFiles <- c("AgMidHiSlopes.csv", "AgriculturalNitrogen.csv",
             "Kffact.csv", "NLCD2011.csv", "PRISM_1981_2010.csv",
             "Runoff.csv", "STATSGO_Set2.csv")


# for loop to read in each .csv file.  About 3 minuts. 
# Commented code is from effort to use read_csv_chunk to only read in lines
# the 11880 observations I need.  Files contain 378,088 records.  I couldn't
# get it to work.
lCatList <- list()  # empty list
# f <- function(x, pos) {x[x$COMID %in% ohKyInWB$COMID, ]} # filter function to be applied below
for (i in 1:length(lcFiles)) {  # loop to read each file
  lcFiles.i <- readr::read_csv(file = paste0("M:/GIS_data/lakeCat/FinalTables/", 
                                             lcFiles[i])
                               #callback = DataFrameCallback$new(f), # this applies function, combined all data into one DF
                               #chunk_size = 50000) # 50,000 records at a time.
  )
  lCatList[[i]] <- lcFiles.i
}

# Merge files
lCat <- do.call("cbind", lCatList)  # Coerces list into dataframe.

# A few fields are redundant across all dataframes (i.e. COMID, CatAreaSqKm,
# WsAreaSqKm, CatPctFull, WsPctFull) and were retained by the cbind call.
#  This code identifies and removes these redundant fields.
lCat <- lCat[, !duplicated(names(lCat))] # eliminate duplicated columns

# 70 variables desribing watersheds.
# includes lakes, ponds, or reservoirs.  estuaries, ice masses, playas excluded
# based on FCode
str(lCat) # 378,088 observations, all US waterbodies

# merge NHD waterbody and lCat
dim(ohKyInRes) # 11,885 observations
dim(lCat) # 378,088 observations, all US waterbodies
ohKyInNatCov <- merge(ohKyInRes, lCat, by = "COMID") # keeps only matched COMID
length(ohKyInNatCov$COMID) # 11,879  observations.  
# 11,885 - 11,879 = 6 OH/KY/IN reservoirs in NHD, not in lCat.  Not sure why.

# Look for FCODE for OH/KY/IN reservoirs not in lCat
filter(ohKyInRes, !(COMID %in% ohKyInNatCov$COMID)) %>%
  select(FCODE) %>% 
  table() 
# 6 from FCODE == 39004 perrenial lake/pond.  This group must be excluded from lCat


# STEP 3: lakeMorpho DATA FOR RESERVOIRS IN OHIO/KENTUCKY/INDIANA----------------
# Data available by hydrologic region.  We need:
resReg <- c("Ohio05", "GreatLakes04", "Tennessee06", "LowerMississippi08")

# for loop to read in each .dbf file.
lMorphList <- list()  # empty list
for (i in 1:length(resReg)) {  # loop to read each file
  lmFiles.i <- foreign::read.dbf(file = paste0("M:/GIS_data/lakeMorpho/", 
                                             resReg[i], ".dbf"))
  lMorphList[[i]] <- lmFiles.i
}

# Merge files
lMorph <- do.call("rbind", lMorphList)  # Coerces list into dataframe.

# Some names got cut when .gdb was extracted to .shp in ArcCatalog.  Also
# some unneeded columns are present.  Clean things up a bit.
lMorph <- lMorph %>% 
  select(-Shape_Leng, -Shape_Area, -nlaSITE_ID, -MaxDepthNL, -ShorelineD,
         -contains("Fetch"), -VolumeCorr)

dim(lMorph) # 59,491 waterbodies in 4 hydrologic regions
dim(ohKyInNatCov) # 11,879 reservoirs in oh/ky/in
ohKyInNatCov <- merge(lMorph, ohKyInNatCov, by = "COMID")
dim(ohKyInNatCov) # 11,193, therefore 11879-11193 = 686 reservoirs not in lakeMorpho

filter(ohKyInRes, !(COMID %in% ohKyInNatCov$COMID)) %>%
  select(FCODE) %>% 
  table() 
# 674 from FCODE == 39004 "perrenial lake/pond".  This group must be excluded from lMorpho

# Any missing depths, or depths <= 0?
filter(ohKyInNatCov, is.na(MeanDepthC) | MeanDepthC <= 0) %>%
  summarise(no.depth = length(MeanDepthC)) # only 10 without depth data.

ohKyInNatCov <- filter(ohKyInNatCov, MeanDepthC > 0) # extract those with depth >0
dim(ohKyInNatCov) # 11,183.  

# STEP 4:  HARMONIZE VARIABLE NAMES WITH THOSE IN gbm--------------------
# Need to harmonize variable names with those used in all other datasets
ohKyInNatCov <- ohKyInNatCov %>%
  rename(max.depth.m = MaxDepthCo,
         mean.depth.m = MeanDepthC,
         reservoir.area.m2 = SurfaceAre,
         nhdPctAg2006Slp10Ws = PctAg2006Slp10Ws,
         nhdManureWs = ManureWs, # manure application (kg N/ha/yr)
         nhdRunoffWs = RunoffWs, # mean runoff (mm)
         nhdKffactWs = KffactWs, # Mean Kffactor within watershed
         nhdTmean8110Ws = Tmean8110Ws, # 30-year normal mean temperature (C°)
         nhdOmWs = OmWs) %>%
  mutate(depth.ratio = mean.depth.m / max.depth.m, 
         dynamic.ratio = sqrt(reservoir.area.m2/1000000)/mean.depth.m, # km2/m
         q.bathy = (max.depth.m/mean.depth.m) - 1,
         prop.less.3m.partA = 1-(3/max.depth.m),
         prop.less.3m.partB = prop.less.3m.partA^q.bathy,
         prop.less.3m = ifelse(max.depth.m <=3,
                               1,
                               1-prop.less.3m.partB),
         watershed.area.m2 = WsAreaSqKm * 1000000,
         rda = watershed.area.m2 / reservoir.area.m2,
         percent.agg.ag = PctHay2011Ws + PctCrop2011Ws)

# Write to disk for Will's use
# write.table(ohKyInNatCov, file.path("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)",
#                                     "multiResSurvey/output/ohKyInNatCov.txt"), row.names = FALSE)


# STEP 5: PREDICT TOTAL CH4 EMISSION RATES RESERVOIRS IN OHIO/KENTUCKY/INDIANA----------------

# Read in table of reservoir characteristics if not already in memory
# ohKyInNatCov <- read.table(file.path("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)",
#                                     "multiResSurvey/output/ohKyInNatCov.txt"), header = TRUE)

# Read in gbm for total CH4, local observations, Nat variables, if not in memory  
# The list specified below contains all models.  
# load("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/multiResSurvey/output/nhdRed2Mse.RData")  

# Select total CH4, local observations, Nat variables
lapply(X = nhdRed2Mse, FUN = function(x) {x$run.name}) # need list itme 8
nhdRed2Mse[[8]]$run.name # confirm this is the right model
litPreds <- predict.gbm(nhdRed2Mse[[8]]$gbm, 
                        newdata = ohKyInNatCov,  
                        n.trees = nhdRed2Mse[[8]]$optTrees) %>%
  cbind(., ohKyInNatCov) %>%
  rename(., litPreds = .)

summary(litPreds$litPreds)
summary(localDataGbm$ch4.trate.mg.h_Estimate)

litPreds <- mutate(litPreds,
                   ch4.trate.mg.d = litPreds * reservoir.area.m2 *24)

# Total daily summertime emissions from KY/IN/OH reservoirs
sum(litPreds$ch4.trate.mg.d)/10^12 # mg/d --> Gg/d

# Total annual summertime emissions from KY/IN/OH reservoirs
# Assuming 7 active months
(sum(litPreds$ch4.trate.mg.d)/10^12) * (30*7) # mg/d --> Gg/season

save(tCh4LocNatMse,file = file.path("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)",
                                    "multiResSurvey/output/tCh4LocNatMse.RData"))

save(litPreds,file = file.path("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)",
                               "multiResSurvey/output/litPreds.RData"))

write.table(litPreds[, c("COMID", "litPreds")],
            file = file.path("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)",
                             "multiResSurvey/output/litPreds.txt"),
            row.names = FALSE)


## PIE CHART FOR OH/KY/IN CH4 EMISSIONS

# Pie Chart with Percentages
slices <- c(352, 97, 24.4, 21.2, 2.1, 0.5, 0.1) 
lbls <- c("landfills", "reservoirs", "natural gas", "power plants", "refineries", "metals", "chemicals")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 

tiff(file.path("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)",
               "multiResSurvey/output/figures/ch4PieChart.tiff"),
     res = 600, compression = "lzw",height = 5, width = 5, units = "in")
pie(slices, col=diverge_hcl(length(lbls)), labels = NA)
dev.off()

