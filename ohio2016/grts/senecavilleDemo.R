# DEMO:  SENECAVILLE (SENECA?) LAKE DESIGN
# SHALLOW LAKE WITH TWO LARGE TRIBUTARIES ON EAST.  WILL STRATIFY BY 
# 'TRIB' AND 'OPEN-WATER'.
# WILL USE A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
senecavilleEqArea <- readOGR(dsn = paste(rootDir, "senecavilleDemo", sep=""), # Could use read.shape from spsurvey package
                           layer = "senecavilleEqArea")  # shapefile name
plot(senecavilleEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attSenecaville <- read.dbf(filename = paste(rootDir, "senecavilleDemo/senecavilleEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
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
                         DesignID="S", # S for stratified
                         type.frame="area",
                         src.frame="shapefile",
                         in.shape=paste(rootDir, "senecavilleDemo/senecavilleEqArea", sep=""),
                         att.frame=attSenecaville,
                         stratum="strata",
                         shapefile=TRUE,
                         out.shape=paste(rootDir, "senecavilleDemo/senecavilleSites", sep=""),
                         prjfilename=paste(rootDir, "senecavilleDemo/senecavilleEqArea", sep=""))


# Print the initial six lines of the survey design
head(senecavilleSites@data)

# Print the survey design summary
summary(senecavilleSites)

# VISUALIZE SURVEY DESIGN-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
senecavilleEqArea84 <- spTransform(x = senecavilleEqArea, 
                                   CRS("+proj=longlat +datum=WGS84")) # specifies projection
senecavilleEqArea84@data$id = rownames(senecavilleEqArea84@data)
senecavilleEqArea84.f <- fortify(senecavilleEqArea84, region="id")  # fortify polygon for ggmap/ggplot
senecavilleEqArea84.f <- merge(senecavilleEqArea84.f, senecavilleEqArea84@data, 
                               by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
senecavilleSitesPlot <- readOGR(dsn = paste(rootDir, "senecavilleDemo", sep=""), 
                             layer = "senecavilleSites")  # shapefile created with grts function
senecavilleSites84 <- spTransform(x = senecavilleSitesPlot, #reproject
                                  CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
senecavilleSites84@data <- mutate(senecavilleSites84@data, 
                                  long=coordinates(senecavilleSites84)[,1], # add long to @data slot
                                  lat=coordinates(senecavilleSites84)[,2]) # add lat to @data slot
# Get ggmap
bbox <- make_bbox(data=senecavilleSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.2) # f is zoom.  Large #, less zoom. tweak for each lake.  
senecavilleSat <- get_map(location = bbox,
                  color = "color",
                  source = "google",
                  maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(senecavilleSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=senecavilleEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(senecavilleSites84@data, panel == "PanelOne"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(senecavilleSites84@data, panel == "PanelOne"), #only main sites
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Main Measurement locations for Senecaville Lake")

ggsave(filename=paste(rootDir, "senecavilleDemo/senecavilleMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(senecavilleSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=senecavilleEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(senecavilleSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(senecavilleSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +
  coord_equal() +
  ggtitle("Oversample locations for Senecaville Lake")

ggsave(filename=paste(rootDir, "senecavilleDemo/senecavilleOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(senecavilleSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=senecavilleEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=senecavilleSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=senecavilleSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Oversample", "Main")) +
  coord_equal() +
  ggtitle("All Measurement locations for Senecaville Lake")

ggsave(filename=paste(rootDir, "senecavilleDemo/senecavilleAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



