# SCRIPT FOR ANALYZING THE GRTS SURVEY CONDUCTED AT caesarCreek

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# WHERE SHAPEFILES ARE STORED
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"


# READ FINAL SHAPEFILE ---------
# This is the point shapefile in equal area projection
caesarCreekData <- readOGR(dsn = paste(rootDir, "caesarCreek", sep=""), # Could use read.shape from spsurvey package
                     layer = "caesarCreekSitesEqAreaData", # shapefile name
                     stringsAsFactors = FALSE)  


# Columns that should be converted to numeric
cols <- c("chm_vol", "wtrDpth", "smDpthS", "Tmp_C_S", "DOPrc_S", "DO__L_S",   
          "SpCn__S", "pH_S", "ORP_S", "TrNTU_S", "chla_S", "smDpthD", "Tmp_C_D", "DOPrc_D", "DO__L_D",   
          "SpCn__D", "pH_D", "ORP_D", "TrNTU_D", "chla_D", "BrPrssr", "TtTrpVl", "LatSamp", "LongSmp")

caesarCreekData@data[, cols] <- as.numeric(unlist(caesarCreekData@data[, cols])) # convert to numeric

# Preview data
ggplot(caesarCreekData@data, aes(siteID, chla_S)) + geom_point() 
ggplot(caesarCreekData@data, aes(siteID, TtTrpVl)) + geom_point()

# ADJUST WEIGHTS
caesarCreekSitesAdj <- ifelse(caesarCreekData@data$EvalStatus == "sampled",
                        TRUE, FALSE)
caesarCreekWgtAdj <- caesarCreekData@data$wgt
caesarCreekWgtCat <- caesarCreekData@data$mdcaty  # mdcaty for unequal probability

# Need to define framesize by mdcaty (section) if unequal probability was used.
owFramesizeAdj1 <- filter(caesarCreekData@data, section == "north") %>%
  distinct(Area_km2) %>% select(Area_km2)

owFramesizeAdj2 <- filter(caesarCreekData@data, section == "south") %>%
  distinct(Area_km2) %>% select(Area_km2)

tribFramesizeAdj <- filter(caesarCreekData@data, section == "Equal") %>% # section == "Equal"
  distinct(Area_km2) %>% select(Area_km2)



caesarCreekFramesizeAdj <- c(owFramesizeAdj1[1,1], owFramesizeAdj2[1,1],
                             tribFramesizeAdj[1,1])
attributes(caesarCreekFramesizeAdj) <- NULL
names(caesarCreekFramesizeAdj) <- c("north", "south", "Equal")

caesarCreekData@data$adjWgt <- adjwgt(caesarCreekSitesAdj, caesarCreekWgtAdj, caesarCreekWgtCat, caesarCreekFramesizeAdj)



# ANALYSIS OF QUANTITATIVE VARIABLE-----------------------
# Framesize by strata!
owFramesize <- filter(caesarCreekData@data, stratum == "open_water") %>%
  select(Area_km2) %>% distinct(Area_km2)
owFramesize <- sum(owFramesize$Area_km2) # sum needed to accomodate multiple mdcaty

tribFramesize <- filter(caesarCreekData@data, stratum == "trib") %>%
  select(Area_km2) %>% distinct(Area_km2)
tribFramesize <- sum(tribFramesize$Area_km2) # sum needed to accomodate multiple mdcaty

framesize <- c("open_water" = owFramesize, "trib" = tribFramesize)

# Determine number of rows in dataframe
caesarCreekNr <- nrow(caesarCreekData@data)

# Create the sites data frame.
sites <- data.frame(siteID=caesarCreekData@data$siteID,
                    Use=caesarCreekData@data$EvalStatus == "sampled")  # use sampled sites

#Create the subpop data frame.
subpop <- data.frame(siteID=caesarCreekData@data$siteID,
                     lake=rep("lake", caesarCreekNr),
                     stratum=caesarCreekData@data$stratum)

#Create the design data frame.
design <- data.frame(siteID=caesarCreekData@data$siteID,
                     wgt=caesarCreekData@data$adjWgt,
                     xcoord=caesarCreekData@data$xcoord,
                     ycoord=caesarCreekData@data$ycoord)

#Create the data.cont data frame.
data.cont <- data.frame(siteID=caesarCreekData@data$siteID,
                        trpVol=caesarCreekData@data$TtTrpVl, # volume of gas in trap
                        chla=caesarCreekData@data$chla_S)

# CDF estimates
caesarCreekCdf <- cont.analysis(sites, subpop, design, data.cont,
                          popsize=list(lake=sum(framesize),
                                       stratum=as.list(framesize)))

filter(caesarCreekCdf$Pct, grepl("Mean", x = Statistic))
