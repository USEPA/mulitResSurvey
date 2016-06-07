# KNOX LAKE DESIGN
# SMALL LAKE WITH TRIBUTARY ON THE NE SIDE
# SHALLOW NE BASIN (<5'); TALKED WITH FISHING MANAGER (6/7/16) WHO REPORTED THAT
# IT IS DIFFICULT TO NAVIGATE ANYWHERE UPSTREAM OF THE TWO BOAT LAUNCHES ON OLD MANSFIELD ROAD
# HE HIMSELF DOESN'T RISK IT, DUE TO STUMPS, SHALLOW WATER
#### knoxEqArea SHAPE FILE WAS MODIFIED TO EXCLUDE APPROX. HALF OF THE NE BASIN 6/7/16 BY SW
# LAKE IS DIVIDED INTO TWO STRATA: 'TRIB' AND 'OPEN-WATER'.
# WILL USE A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
knoxEqArea <- readOGR(dsn = paste(rootDir, "knox", sep=""), # Could use read.shape from spsurvey package
                       layer = "knoxEqArea")  # shapefile name
plot(knoxEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attKnox <- read.dbf(filename = paste(rootDir, "knox/knoxEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### with the modified knoxEqArea polygon, the trib is 0.75 km2, and the open water is 0.62 km2
### set the number of main sites in the tributary area to 5 -- 
### more than half the lake, but much of it still may be inaccessable

knoxDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                      seltype="Equal",
                                      over=20),
                  "trib"=list(panel=c(mainSites=5),
                              seltype="Equal",
                              over=15))

knoxSites <- grts(design=knoxDsgn,
                   DesignID="S", # S for stratified
                   type.frame="area",
                   src.frame="shapefile",
                   in.shape=paste(rootDir, "knox/knoxEqArea", sep=""),
                   att.frame=attKnox,
                   stratum="strata",
                   shapefile=TRUE,
                   out.shape=paste(rootDir, "knox/knoxSites", sep=""),
                   prjfilename=paste(rootDir, "knox/knoxEqArea", sep=""))


# Print the initial six lines of the survey design
head(knoxSites@data)

# Print the survey design summary
summary(knoxSites)

# simple plotting
plot(knoxEqArea)
points(knoxSites$xcoord, knoxSites$ycoord)

# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
knoxEqArea84 <- spTransform(x = knoxEqArea, 
                             CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = knoxEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "knox", sep=""), 
         layer = "knoxEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
knoxEqArea84@data$id = rownames(knoxEqArea84@data)
knoxEqArea84.f <- fortify(knoxEqArea84, region="id")  # fortify polygon for ggmap/ggplot
knoxEqArea84.f <- merge(knoxEqArea84.f, knoxEqArea84@data, 
                         by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plottin
knoxSitesPlot <- readOGR(dsn = paste(rootDir, "knox", sep=""), 
                          layer = "knoxSites")  # shapefile created with grts function
knoxSites84 <- spTransform(x = knoxSitesPlot, #reproject
                            CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
knoxSites84@data <- mutate(knoxSites84@data, 
                            long=coordinates(knoxSites84)[,1], # add long to @data slot
                            lat=coordinates(knoxSites84)[,2], # add lat to @data slot
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
write.table(filter(knoxSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "knox/knoxOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = knoxSites84, 
         dsn = paste(rootDir, "knox", sep=""), 
         layer = "knoxSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
knoxSites84ListByPanel <- split(knoxSites84, # split preserves class, outputs a list
                                 f= knoxSites84@data$panel)

# to look at the list (optional):
knoxSites84ListByPanel[[1]]@data        #should be mainSites
knoxSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = knoxSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "knox", sep=""), 
         layer = "knoxSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = knoxSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "knox", sep=""), 
         layer = "knoxSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=knoxSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.15) # f is zoom.  Large #, less zoom. tweak for each lake.  
knoxSat <- get_map(location = bbox,
                    color = "color",
                    source = "google",
                    maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(knoxSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=knoxEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(knoxSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(knoxSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Knox Lake")

ggsave(filename=paste(rootDir, "knox/knoxMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(knoxSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=knoxEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(knoxSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(knoxSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Knox Lake")

ggsave(filename=paste(rootDir, "knox/knoxOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(knoxSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=knoxEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=knoxSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=knoxSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Knox Lake")

ggsave(filename=paste(rootDir, "knox/knoxAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

