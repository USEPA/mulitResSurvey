# SCRIPT FOR ANALYZING THE GRTS SURVEY CONDUCTED AT COWAN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# WHERE SHAPEFILES ARE STORED
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"


# READ FINAL SHAPEFILE ---------
# This is the point shapefile in equal area projection
cowanData <- readOGR(dsn = paste(rootDir, "cowan", sep=""), # Could use read.shape from spsurvey package
                     layer = "cowanSitesEqAreaData", # shapefile name
                     stringsAsFactors = FALSE)  


# Columns that should be converted to numeric
cols <- c("chm_vol", "wtrDpth", "smDpthS", "Tmp_C_S", "DOPrc_S", "DO__L_S",   
          "SpCn__S", "pH_S", "ORP_S", "TrNTU_S", "chla_S", "smDpthD", "Tmp_C_D", "DOPrc_D", "DO__L_D",   
          "SpCn__D", "pH_D", "ORP_D", "TrNTU_D", "chla_D", "BrPrssr", "TtTrpVl", "LatSamp", "LongSmp")

cowanData@data[, cols] <- as.numeric(unlist(cowanData@data[, cols])) # convert to numeric

# Preview data
ggplot(cowanData@data, aes(siteID, chla_S)) + geom_point() # one outlier
cowanData@data[cowanData@data$chla_S > 400 & !is.na(cowanData@data$chla_S), "chla_S"] = NA
ggplot(cowanData@data, aes(siteID, TtTrpVl)) + geom_point() # one outlier

# ADJUST WEIGHTS
cowanSitesAdj <- ifelse(cowanData@data$EvalStatus == "sampled",
                        TRUE, FALSE)
cowanWgtAdj <- cowanData@data$wgt
cowanWgtCat <- cowanData@data$stratum

owFramesizeAdj <- filter(cowanData@data, stratum == "open_water") %>%
  distinct(Area_km2) %>% select(Area_km2)

tribFramesizeAdj <- filter(cowanData@data, stratum == "trib") %>%
  distinct(Area_km2) %>% select(Area_km2)


cowanFramesizeAdj <- c(owFramesizeAdj[1,1], tribFramesizeAdj[1,1])
attributes(cowanFramesizeAdj) <- NULL
names(cowanFramesizeAdj) <- c("open_water", "trib")

cowanData@data$adjWgt <- adjwgt(cowanSitesAdj, cowanWgtAdj, cowanWgtCat, cowanFramesizeAdj)



# ANALYSIS OF QUANTITATIVE VARIABLE-----------------------
# Framesize
owFramesize <- filter(cowanData@data, stratum == "open_water") %>%
  select(Area_km2) %>% distinct(Area_km2)
owFramesize <- owFramesize$Area_km2

tribFramesize <- filter(cowanData@data, stratum == "trib") %>%
  select(Area_km2) %>% distinct(Area_km2)
tribFramesize <- tribFramesize$Area_km2

framesize <- c("open_water" = owFramesize, "trib" = tribFramesize)

# Determine number of rows in dataframe
cowanNr <- nrow(cowanData@data)

# Create the sites data frame.
sites <- data.frame(siteID=cowanData@data$siteID,
                    Use=cowanData@data$EvalStatus == "sampled")  # use sampled sites

#Create the subpop data frame.
subpop <- data.frame(siteID=cowanData@data$siteID,
                     lake=rep("lake", cowanNr),
                     stratum=cowanData@data$stratum)

#Create the design data frame.
design <- data.frame(siteID=cowanData@data$siteID,
                     wgt=cowanData@data$adjWgt,
                     xcoord=cowanData@data$xcoord,
                     ycoord=cowanData@data$ycoord)

#Create the data.cont data frame.
data.cont <- data.frame(siteID=cowanData@data$siteID,
                       trpVol=cowanData@data$TtTrpVl, # volume of gas in trap
                       chla=cowanData@data$chla_S)

# CDF estimates
cowanCdf <- cont.analysis(sites, subpop, design, data.cont,
                               popsize=list(lake=sum(framesize),
                                            stratum=as.list(framesize)))

filter(cowanCdf$Pct, grepl("Mean", x = Statistic))
