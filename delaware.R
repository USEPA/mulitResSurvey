# DELAWARE RESERVOIR DESIGN
# MEDIUM LAKE (4.25 KM2) WITH TWO TRIBUTARIES ON THE N SIDE
# BOTH TRIBUTARY ARMS GET SHALLOW (<5'); TALKED WITH BEN ODELLE OF USACE (6/7/16) WHO REPORTED THAT
# THE LAKE IS CURRENTLY 5.5' ABOVE WINTER POOL, A BIT HIGER THAN THE TYPICAL SUMMER POOL

#### delawareEqArea SHAPE FILE WAS MODIFIED TO EXCLUDE THE FURTHEST UPSTREAM PARTS OF THE TRIBS ON 6/7/16 BY SW
# LAKE IS DIVIDED INTO TWO STRATA: 'TRIB' AND 'OPEN-WATER'.
# WILL USE A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
delawareEqArea <- readOGR(dsn = paste(rootDir, "delaware", sep=""), # Could use read.shape from spsurvey package
                      layer = "delawareEqArea")  # shapefile name
plot(delawareEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attDelaware <- read.dbf(filename = paste(rootDir, "delaware/delawareEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### with the modified delawareEqArea polygon, the trib is 1.45 km2, and the open water is 2.79 km2
### set the number of main sites in the tributary area to 7 -- 
### higher sampling density in the trib (4.8/km2) than in the open water (2.9/km2) 

delawareDsgn <- list("open_water" = list(panel=c(mainSites=8),
                                     seltype="Equal",
                                     over=20),
                 "trib"=list(panel=c(mainSites=7),
                             seltype="Equal",
                             over=15))

delawareSites <- grts(design=delawareDsgn,
                  DesignID="S", # S for stratified
                  type.frame="area",
                  src.frame="shapefile",
                  in.shape=paste(rootDir, "delaware/delawareEqArea", sep=""),
                  att.frame=attDelaware,
                  stratum="strata",
                  shapefile=TRUE,
                  out.shape=paste(rootDir, "delaware/delawareSites", sep=""),
                  prjfilename=paste(rootDir, "delaware/delawareEqArea", sep=""))


# Print the initial six lines of the survey design
head(delawareSites@data)

# Print the survey design summary
summary(delawareSites)

# simple plotting
plot(delawareEqArea)
points(delawareSites$xcoord, delawareSites$ycoord)

# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
delawareEqArea84 <- spTransform(x = delawareEqArea, 
                            CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = delawareEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "delaware", sep=""), 
         layer = "delawareEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
delawareEqArea84@data$id = rownames(delawareEqArea84@data)
delawareEqArea84.f <- fortify(delawareEqArea84, region="id")  # fortify polygon for ggmap/ggplot
delawareEqArea84.f <- merge(delawareEqArea84.f, delawareEqArea84@data, 
                        by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plottin
delawareSitesPlot <- readOGR(dsn = paste(rootDir, "delaware", sep=""), 
                         layer = "delawareSites")  # shapefile created with grts function
delawareSites84 <- spTransform(x = delawareSitesPlot, #reproject
                           CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
delawareSites84@data <- mutate(delawareSites84@data, 
                           long=coordinates(delawareSites84)[,1], # add long to @data slot
                           lat=coordinates(delawareSites84)[,2], # add lat to @data slot
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
write.table(filter(delawareSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "delaware/delawareOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = delawareSites84, 
         dsn = paste(rootDir, "delaware", sep=""), 
         layer = "delawareSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
delawareSites84ListByPanel <- split(delawareSites84, # split preserves class, outputs a list
                                f= delawareSites84@data$panel)

# to look at the list (optional):
delawareSites84ListByPanel[[1]]@data        #should be mainSites
delawareSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = delawareSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "delaware", sep=""), 
         layer = "delawareSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = delawareSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "delaware", sep=""), 
         layer = "delawareSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=delawareSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 2) # f is zoom.  Large #, less zoom. tweak for each lake.  
delawareSat <- get_map(location = bbox,
                   color = "color",
                   source = "google",
                   maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(delawareSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=delawareEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(delawareSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(delawareSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Delaware Reservoir")

ggsave(filename=paste(rootDir, "delaware/delawareMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(delawareSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=delawareEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(delawareSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(delawareSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Delaware Reservoir")

ggsave(filename=paste(rootDir, "delaware/delawareOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(delawareSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=delawareEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=delawareSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=delawareSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Delaware Reservoir")

ggsave(filename=paste(rootDir, "delaware/delawareAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
