# COWAN LAKE DESIGN
# SMALL LAKE WITH TORTUOUS TRIBUTARY ON THE EAST SIDE
# SHAPE FILE POLYGON WAS INCONSISTENT WITH SATELLITE IMAGERY OF THE LAKE,
#### cowanEqArea SHAPE FILE WAS MODIFIED TO EXCLUDE NON-LAKE AREAS ON 5/26/16 BY JB & SW
# LAKE IS DIVIDED INTO TWO STRATA: 'TRIB' AND 'OPEN-WATER'.
# WILL USE A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
cowanEqArea <- readOGR(dsn = paste(rootDir, "cowan", sep=""), # Could use read.shape from spsurvey package
                             layer = "cowanEqArea")  # shapefile name
plot(cowanEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attCowan <- read.dbf(filename = paste(rootDir, "cowan/cowanEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 3, since it is a small area (.25 sq km)
### according to the shape file, and likely a smaller area of it is accessible
cowanDsgn <- list("open_water" = list(panel=c(mainSites=12),
                                            seltype="Equal",
                                            over=20),
                        "trib"=list(panel=c(mainSites=3),
                                    seltype="Equal",
                                    over=9))

cowanSites <- grts(design=cowanDsgn,
                         DesignID="S", # S for stratified
                         type.frame="area",
                         src.frame="shapefile",
                         in.shape=paste(rootDir, "cowan/cowanEqArea", sep=""),
                         att.frame=attCowan,
                         stratum="strata",
                         shapefile=TRUE,
                         out.shape=paste(rootDir, "cowan/cowanSites", sep=""),
                         prjfilename=paste(rootDir, "cowan/cowanEqArea", sep=""))


# Print the initial six lines of the survey design
head(cowanSites@data)

# Print the survey design summary
summary(cowanSites)

# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
cowanEqArea84 <- spTransform(x = cowanEqArea, 
                                   CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = cowanEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "cowan", sep=""), 
         layer = "cowanEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
cowanEqArea84@data$id = rownames(cowanEqArea84@data)
cowanEqArea84.f <- fortify(cowanEqArea84, region="id")  # fortify polygon for ggmap/ggplot
cowanEqArea84.f <- merge(cowanEqArea84.f, cowanEqArea84@data, 
                               by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plottin
cowanSitesPlot <- readOGR(dsn = paste(rootDir, "cowan", sep=""), 
                                layer = "cowanSites")  # shapefile created with grts function
cowanSites84 <- spTransform(x = cowanSitesPlot, #reproject
                                  CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
cowanSites84@data <- mutate(cowanSites84@data, 
                                  long=coordinates(cowanSites84)[,1], # add long to @data slot
                                  lat=coordinates(cowanSites84)[,2], # add lat to @data slot
                                  deployDate = "",    # adding all of these colums to the 
                                  deployTime = "",    # shape file to be filled in the field
                                  waterDepth = "",    # tried to enter them in the order they will be filled                      
                                  Temp_F_S = "",
                                  DOPercent_S = "",
                                  DO_mg_L_S = "",
                                  SpCond_ms_S = "",
                                  pH_S = "",
                                  ORP_S = "",
                                  TurbidNTU_S = "",
                                  chla_S = "",
                                  Temp_F_D = "",
                                  DOPercent_D = "",
                                  DO_mg_L_D = "",
                                  SpCond_ms_D = "",
                                  pH_D = "",
                                  ORP_D = "",
                                  TurbidNTU_D = "",
                                  chla_D = "",
                                  AirExtnrs = "",
                                  DG_Extnrs = "",
                                  BarPrssr = "",
                                  RtrvDate = "",
                                  RtrvTime = "",
                                  TotTrapVol = "",
                                  TrapExtnrs = "",
                                  Notes = ""
)

# write out table of overdraw sites for reference in field
write.table(filter(cowanSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "cowan/cowanOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = cowanSites84, 
         dsn = paste(rootDir, "cowan", sep=""), 
         layer = "cowanSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
cowanSites84ListByPanel <- split(cowanSites84, # split preserves class, outputs a list
                                       f= cowanSites84@data$panel)

# to look at the list (optional):
cowanSites84ListByPanel[[1]]@data        #should be mainSites
cowanSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = cowanSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "cowan", sep=""), 
         layer = "cowanSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = cowanSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "cowan", sep=""), 
         layer = "cowanSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=cowanSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.5) # f is zoom.  Large #, less zoom. tweak for each lake.  
cowanSat <- get_map(location = bbox,
                          color = "color",
                          source = "google",
                          maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(cowanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=cowanEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(cowanSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(cowanSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Cowan Lake")

ggsave(filename=paste(rootDir, "cowan/cowanMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(cowanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=cowanEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(cowanSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(cowanSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Cowan Lake")

ggsave(filename=paste(rootDir, "cowan/cowanOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(cowanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=cowanEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=cowanSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=cowanSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Cowan Lake")

ggsave(filename=paste(rootDir, "cowan/cowanAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")


