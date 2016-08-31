# SCRIPT FOR ANALYZING THE GRTS SURVEY CONDUCTED AT COWAN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# WHERE SHAPEFILES ARE STORED
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"


# READ FINAL SHAPEFILE ---------
# This is the point shapefile in equal area projection
cowanData <- readOGR(dsn = paste(rootDir, "cowan", sep=""), # Could use read.shape from spsurvey package
                        layer = "cowanSitesEqAreaData")  # shapefile name

# Determine number of rows in dataframe
cowanNr <- nrow(cowanData@data)


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

adjwgt(cowanSitesAdj, cowanWgtAdj, cowanWgtCat, cowanFramesizeAdj)



# ANALYSIS OF SITE STATUS EVALUATION VARIABLE-----------------------
# Evaluation status indicates whether a site is in the population or not.
# This information can be used to estimate the size of the resource, which
# is then used to to correct design estimates via the 'popsize' argument.


addmargins(table(SC_estuaries$Status)) # look at # of sampled and NonTarget sites

# Create the sites data frame.
sites <- data.frame(siteID=SC_estuaries$siteID,
                    Use=rep(TRUE, nr))  # use sampled AND nonTarget sites

#Create the subpop data frame.
subpop <- data.frame(siteID=SC_estuaries$siteID,
                     All_Estuaries=rep("All Estuaries", nr),
                     Estuary_Type=SC_estuaries$Stratum)

#Create the design data frame.
design <- data.frame(siteID=SC_estuaries$siteID,
                     wgt=SC_estuaries$wgt,
                     xcoord=SC_estuaries$xcoord,
                     ycoord=SC_estuaries$ycoord)

#Create the data.cat data frame.
data.cat <- data.frame(siteID=SC_estuaries$siteID,
                       Status=SC_estuaries$Status)  # categorical variable to be evaluated

# Calculate extent estimates for the site status evaluation variables
Extent_Estimates <- cat.analysis(sites, subpop, design, data.cat)
print(Extent_Estimates)

