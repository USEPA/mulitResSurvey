## SCRIPT TO READ IN 1) NHDPlusV2 (DOWNLOADED 12/2018) WHICH CONTAINS THE 
## lakeMorpho DATA, 2) lakeCat WHICH CONTAINS THE WATERSHED INFO FOR WATERBODIES,
## AND streamCat WHICH CONTAINS THE WATERSHED INFO FOR STREAMS/RIVERS (CARR CR).

# DEFINE COMID----------------
# Each site has a unique COMID in NHD data.  I identified COMID from manual 
# comparison of NHDPlusV2 with lake shapefiles put together by Pegasus.  Note 
# that Carr Cr. reservoir is coded as river/stream in NHDPlusV2, therefore no 
# lakeCat data.  
nhdComId <- {
  c("Acton Lake", "ACN", 3881594,
    "Alum Creek Lake", "ALC", 5212313, # GNIS_NAME = Williams Lake
    "Apple Valley Lake", "AVL", 15400440,        
    "Atwood Lake", "ATW", 19387454,
    "Brookville Lake", "BVR", 166899200,
    "Buckhorn Lake", "BHR", 486928,            
    "Burr Oak Reservoir", "BOR", 15417757, # no GNIS_NAME
    "Caesar Creek Lake", "CCK", 166899204, # no GNIS_NAME
    "Carr Fork Lake", "CFK", 456124, # stream/river in NHDPlusV2. NOT A NHD WATERBODY!            
    "Cave Run Lake", "CRR", 166830448, # GNIS_NAME = Giant Canada Goose Pond
    "Charles Mill Lake", "CML", 15408897, # no GNIS_NAME
    "Cowan Lake", "CWN", 3932186,              
    "Delaware Reservoir", "DEL", 5213645,
    "Dillon Lake", "DIL", 166899180,
    "Hocking County Lake", "HOK", 15417761, # no GNIS_NAME     
    "Kiser Lake", "KIS", 3980812,
    "Knox Lake", "KNX", 167484602,
    "La Due Reservoir", "LDR", 15585938, # GNIS_NAME = Akron City Reservoir      
    "Lake Loramie", "LOR", 3982320, 
    "Lake Milton", "MIL", 13152651,
    "Lake Mohawk", "MHK", 19387410,              
    "Lake Roaming Rock", "RRK", 9846027,
    "Lake Waynoka", "WKA", 1918274,
    "Michael J Kirwan Reservoir", "MJK", 13155459, # no GNIS_NAME 
    "Paint Creek Lake", "PTC", 5231436, # no GNIS_NAME 
    "Piedmont Lake", "PDT", 19390850,
    "Pleasant Hill Lake", "PHL", 15408959,        
    "Rocky Fork Lake", "RFL", 5231452, # no GNIS_NAME
    "Senecaville Lake", "SNC", 15372030,
    "Tappan Lake", "TPN", 19390686,              
    "William H Harsha Lake", "EFR", 166899203, # no GNIS_NAME
    "Wingfoot Lake", "WGF", 15586832,
    "Allatoona", NA, 6495140,
    "Douglas", NA, 166997463,
    "Fontana", NA, 166912876,
    "Guntersville", NA, 167182204,
    "Hartwell", NA, 166759840,
    "Watts Bar", NA, 166997494, # no GNIS_NAME
    "J.C. Boyle", NA, 358641, # GNIS_NAME = John C Boyle Reservoir
    "Kachess", NA, 24136753, # GNIS_NAME = Kachess Lake
    "Lacamas", NA, 23740719,
    "Foster", NA, 23788981, 
    "Cle Elum", NA, 24136807
  ) 
}

# Coerce to data.frame  
nhdComIdDf <- data.frame(Lake_Name = nhdComId[seq(1,length(nhdComId), 3)],
                         COMID = as.numeric(nhdComId[seq(3,length(nhdComId), 3)]),
                         stringsAsFactors = FALSE)

# NHD DATA-------------------------
# APPROACH
# Original data are stored in geodatabase:
# M:\GIS_data\NHDPlusV2\NHDPlusNationalData\NHDPlusV21_National_Seamless.gdb
# The embedded feature class holding the waterbodies (NHDSnapshot/NHDWaterbody)
# is huge!  I can read it in using SF in about 4 minutes, but adding sf
# to packrat takes forever(!) and I don't really need all the spatial data.
# As a workaround, I extracted the feature class as a shapefile:
# M:\GIS_data\NHDPlusV2\NHDWaterbodyForR.shp 
# and just read in the .dbf file below.  This is superfast.  The sf approach 
# is shown below for reference.

# LIBRARIES
# library(sf) # see notes above

# READ DATA
# With sf. Read NHDPlusV2 for entire country.  About 4 minutes with sf.
# nhd <- sf::st_read(dsn = "M:/GIS_data/NHDPlusV2/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb", 
#                    layer = "NHDWaterbody")

# Just read .dbf
nhd <- foreign::read.dbf(file = "M:/GIS_data/NHDPlusV2/NHDWaterbodyForR.dbf", 
                         as.is = TRUE)


# Filter records from NHDPlusV2
nhdSubset <- filter(nhd, COMID %in% nhdComIdDf$COMID)

length(nhdComIdDf$COMID) # 43 sites
length(nhdSubset$COMID) # 42 sites.  No data for Carr which isn't represented in NHDPlusV2.

# # If the sf approach is used, the final filtered shapefile can be written out.
# dsn <- "C:/Users/JBEAULIE/GitRepository/mulitResSurvey/ohio2016/inputData/watershedAndMorphology/"
# st_write(nhdSubset, paste0(dsn, "nhdSubset.shp"), delete_layer = TRUE)

# streamCat-----------
# Data downloaded 12/11/2018 and are used for Carr Cr. reservoir, which is coded
# as stream/river in NHD.

# List of .CSV files containing the streamCat data
scFiles <- list.files("ohio2016/inputData/watershedAndMorphology/nhdPlus/streamCat", 
                      pattern="*.csv$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file

# for loop to read in each .csv file.  Using read_csv_chunk
# to only read in lines I need.  Files contain 44,467 records, I only need
# those for this study.  This is much faster than reading all records into
# memory, then filtering.
scList <- list() # 29 seconds, wow!
f <- function(x, pos) x[x$COMID == 456124, ] # filter function to be applied below
for (i in 1:length(scFiles)) {  # loop to read each file
  scFiles.i <- read_csv_chunked(paste0("ohio2016/inputData/watershedAndMorphology/nhdPlus/streamCat/", 
                                       scFiles[i]),
                                DataFrameCallback$new(f), # this applies function, combined all data into one DF
                                chunk_size = 50000)
  scList[[i]] <- scFiles.i
}

# Merge files
sCat <- do.call("cbind", scList)  # Coerces list into dataframe.
# A few fields are redundant across all dataframes (i.e. COMID, CatAreaSqKm,
# WsAreaSqKm, CatPctFull, WsPctFull) and were retained by the cbind call.
#  This code identifies and removes these redundant fields.
sCat <- sCat[, !duplicated(names(sCat))] # eliminate duplicated columns

# 330 variables desribing Carr Creek's watershed!  See which ones
# match up with those in lakeCat data.
dim(sCat) 

# lakeCat-------------------------
# lakeCat data downloaded 12/11/2018.  Contains data for all reservoirs
# except Carr Cr. which is in streamCat.

# List of .CSV files containing the streamCat data
lcFiles <- list.files("ohio2016/inputData/watershedAndMorphology/nhdPlus/lakeCat", 
                      pattern="*.csv$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file

# for loop to read in each .csv file.  About 3 minuts. Using read_csv_chunk
# to only read in lines I need.  Files contain 378,088 records, I only need
# those for this study.  This is much faster than reading all records into
# memory, then filtering.
lcList <- list()  # empty list
f <- function(x, pos) x[x$COMID %in% nhdComIdDf$COMID, ] # filter function to be applied below
for (i in 1:length(lcFiles)) {  # loop to read each file
  lcFiles.i <- readr::read_csv_chunked(file = paste0("ohio2016/inputData/watershedAndMorphology/nhdPlus/lakeCat/", 
                                                     lcFiles[i]),
                                       DataFrameCallback$new(f), # this applies function, combined all data into one DF
                                       chunk_size = 50000) # 50,000 records at a time.
  lcList[[i]] <- lcFiles.i
}

# Merge files
sCat <- do.call("cbind", lcList.1)  # Coerces list into dataframe.
# A few fields are redundant across all dataframes (i.e. COMID, CatAreaSqKm,
# WsAreaSqKm, CatPctFull, WsPctFull) and were retained by the cbind call.
#  This code identifies and removes these redundant fields.
lCat <- sCat[, !duplicated(names(sCat))] # eliminate duplicated columns

# 276 variables desribing watersheds.
str(lCat)

# Merge files-----------------
# Lets put together lakeCat and streamCat first #-#-#-#-#-#-#-#-#-#-#-#-
# sCat contains variables not present in lCat.  Lets subset sCat to common
# variables.
name.i <- names(sCat) %in% names(lCat) # index for sCat names in lCat
sCat <- select_if(sCat, name.i) # a bit long, but selects columns where TRUE # code broke here 2/11/19.  using save object in WS

# Check for names in lCat, but not in sCat
# sCat missing data from PRISM_2008_2009_KY.csv.  According to documentation,
# these data should be there (not on streamCat FTP site:
# ftp://newftp.epa.gov/EPADataCommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/)
# These are climate data for 2008 and 2009.  Just use 30-year mean climate data
# from PRISM_1981_2010_KY.csv
names(lCat)[!(names(lCat) %in% names(sCat))] # missing fields
dim(sCat) # 265 columns
dim(lCat) # 276 columns
lCat <- lCat[, names(lCat) %in% names(sCat)] # remove missing fields
dim(lCat) # now 265 columns, good
s_lCat <- rbind(sCat, lCat) # merge.
dim(s_lCat) # 43 x 265, good

# Now merge in balance of NHD data
nhd_all <- Reduce(function(...) merge(..., all = TRUE), # 43 rows, 287 columns
                  list(s_lCat, nhdSubset, nhdComIdDf)) %>%
  # Append all variable names with nhd to differentiate from other data sources
  rename_all(function(x) paste0("nhd", x))  %>%
  rename(Lake_Name = nhdLake_Name)  # remove 'nhd' to facilitate merge w/ meanVariance.c.lake.lu

# Merge w/meanVariance.c.lake.lu
dim(nhd_all) # 43 286
dim(meanVariance.c.lake.lu) # 46 116
meanVariance.c.lake.lu <- merge(meanVariance.c.lake.lu, nhd_all, all = TRUE)
dim(meanVariance.c.lake.lu) # 46 401, good!


# Next, need to deal with 2017 Acton data.  The Lake_Name used for these
# data are Acton Lake July, Acton Lake Aug, and Acton Lake Oct.  These names
# don't match up with NHD data, so many columns are empty. This code populates
# those rows w/ values from Acton Lake.
varNames <- names(nhd_all)[-which(names(nhd_all) == "Lake_Name")] # list of values to be populated (Dont change Lake_Name!)
meanVariance.c.lake.lu[grepl(c("July|Aug|Oct"), meanVariance.c.lake.lu$Lake_Name), varNames] = # NA data
  meanVariance.c.lake.lu[meanVariance.c.lake.lu$Lake_Name == "Acton Lake", varNames]  # replacement data

