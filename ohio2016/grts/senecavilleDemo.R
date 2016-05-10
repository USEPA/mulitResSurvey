# DEMO:  SENECAVILLE (SENECA?) LAKE DESIGN
# SHALLOW LAKE WITH TWO LARGE TRIBUTARIES ON EAST.  WILL STRATIFY BY 
# 'TRIB' AND 'OPEN-WATER'.
# WILL USE A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ/PLOT SHAPEFILE
senecavilleEqArea <- readOGR(dsn = paste(rootDir, "senecavilleDemo", sep=""), # Could use read.shape from spsurvey package
                           layer = "senecavilleEqArea")  # shapefile name
plot(senecavilleEqArea) # visualize polygon

# CREATE SHAPEFILE
sp2shape(sp.obj=senecavilleEqArea, # object created with readOGR or read.shape
         prjfilename = paste(rootDir, "senecavilleDemo/senecavilleEqArea", sep=""), # proj from original shapefile
         shpfilename=paste(rootDir, "senecavilleDemo/senecavilleEqAreaShp", sep="")) # name of created shapefile.

# EXTRACT ATTRIBUTE TABLE
attSenecaville <- read.dbf(filename = paste(rootDir, "senecavilleDemo/senecavilleEqArea", sep=""))


# SET UP FOR GRTS FUNCTION  
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
senecavilleDsgn <- list("open_water" = list(panel=c(PanelOne=10),
                                            seltype="Equal",
                                            over=20),
                        "trib"=list(panel=c(PanelOne=5),
                                    seltype="Equal",
                                    over=20))
senecavilleSites <- grts(design=senecavilleDsgn,
                         DesignID="STRATIFIED",
                         type.frame="area",
                         src.frame="shapefile",
                         in.shape=paste(rootDir, "senecavilleDemo/senecavilleEqAreaShp", sep=""),
                         att.frame=attSenecaville,
                         stratum="strata",
                         shapefile=TRUE,
                         out.shape=paste(rootDir, "senecavilleDemo/senecavilleSites", sep=""),
                         prjfilename=paste(rootDir, "senecavilleDemo/senecavilleEqArea", sep=""))


# Print the initial six lines of the survey design
head(senecavilleSites@data)

# Print the survey design summary
summary(senecavilleSites)

# visualize survey design
senecavilleSitesPanelOne <- filter(senecavilleSites@data, panel=="PanelOne") # extract main sites
senecavilleSitesOver <- filter(senecavilleSites@data, panel=="OverSamp") # extract oversample sites
plot(senecavilleEqArea) #plot polygon
points(senecavilleSitesPanelOne$xcoord, # plot main sites 
       senecavilleSitesPanelOne$ycoord, 
       pch=16, col="red", cex=3, ) # specify symbol type (circle w/fill), color, and size.
points(senecavilleSitesOver$xcoord, 
       senecavilleSitesOver$ycoord, 
       pch=16, col="black") # specify symbol type (circle w/fill), color, and size.

