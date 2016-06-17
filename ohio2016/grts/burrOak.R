# BURR OAK RESERVOIR DESIGN
# SMALL LAKE (2.5 KM2) WITH ONE MAJOR TRIBUTARIE ON NE SIDE

# burrOakEqArea SHAPE FILE WAS MODIFIED TO EXCLUDE THE FURTHEST UPSTREAM PARTS OF THE TRIBS ON 6/7/16 BY SW
# LAKE IS DIVIDED INTO TWO STRATA: 'TRIB' AND 'OPEN-WATER'.
# WILL USE A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
burrOakEqArea <- readOGR(dsn = paste(rootDir, "burrOak", sep=""), # Could use read.shape from spsurvey package
                          layer = "burrOakEqArea")  # shapefile name
plot(burrOakEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attburrOak <- read.dbf(filename = paste(rootDir, "burrOak/burrOakEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### with the modified burrOakEqArea polygon, the trib is 1.45 km2, and the open water is 2.79 km2
### set the number of main sites in the tributary area to 7 -- 
### higher sampling density in the trib (4.8/km2) than in the open water (2.9/km2) 

burrOakDsgn <- list("open_water" = list(panel=c(mainSites=8),
                                         seltype="Equal",
                                         over=10),
                     "trib"=list(panel=c(mainSites=7),
                                 seltype="Equal",
                                 over=10))

burrOakSites <- grts(design=burrOakDsgn,
                      DesignID="S", # S for stratified
                      type.frame="area",
                      src.frame="shapefile",
                      in.shape=paste(rootDir, "burrOak/burrOakEqArea", sep=""),
                      att.frame=attburrOak,
                      stratum="strata",
                      shapefile=TRUE,
                      out.shape=paste(rootDir, "burrOak/burrOakSitesEqArea", sep=""),
                      prjfilename=paste(rootDir, "burrOak/burrOakEqArea", sep=""))


# Print the initial six lines of the survey design
head(burrOakSites@data)

# Print the survey design summary
summary(burrOakSites)

# simple plotting
plot(burrOakEqArea)
points(burrOakSites$xcoord, burrOakSites$ycoord)

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able data fields.
# We want these fields in the Equal Area and WGS84 projections of this shapefile.
# Need to read in shapefile created with grts function, then modify, then write back to disk.
burrOakSitesEqArea <- readOGR(dsn = paste(rootDir, "burrOak", sep=""), # Could use read.shape from spsurvey package
                              layer = "burrOakSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
burrOakSitesEqArea@data <- mutate(burrOakSitesEqArea@data, 
                                  deplyDt = "",    # adding all of these colums to the 
                                  deplyTm = "",    # shape file to be filled in the field
                                  chmStTm = "",    # tried to enter them in the order they will be filled
                                  chm_vol = "",
                                  bbblngO = "",                                
                                  wtrDpth = "", 
                                  smDpthS = "",
                                  Tmp_C_S = "",
                                  DOPrc_S = "",
                                  DO__L_S = "",
                                  SpCn__S = "",
                                  pH_S = "",
                                  ORP_S = "",
                                  TrNTU_S = "",
                                  chla_S = "",
                                  smDpthD = "",
                                  Tmp_C_D = "",
                                  DOPrc_D = "",
                                  DO__L_D = "",
                                  SpCn__D = "",
                                  pH_D = "",
                                  ORP_D = "",
                                  TrNTU_D = "",
                                  chla_D = "",
                                  ArExtnrs = "",
                                  DG_Extn = "",
                                  H2O_vol = "",
                                  HeVol = "",
                                  BrPrssr = "",
                                  RtrvDat = "",
                                  RtrvTim = "",
                                  TtTrpVl = "",
                                  TrapExtn = "",
                                  Notes = "",
                                  LatSamp = "",
                                  LongSmp = ""
)



# re-write this mutated file, will keep the equal area projection
writeOGR(obj = burrOakSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "burrOak", sep=""), 
         layer = "burrOakSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
burrOakEqArea84 <- spTransform(x = burrOakEqArea, 
                                CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = burrOakEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "burrOak", sep=""), 
         layer = "burrOakEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
burrOakEqArea84@data$id = rownames(burrOakEqArea84@data)
burrOakEqArea84.f <- fortify(burrOakEqArea84, region="id")  # fortify polygon for ggmap/ggplot
burrOakEqArea84.f <- merge(burrOakEqArea84.f, burrOakEqArea84@data, 
                            by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
burrOakSitesPlot <- readOGR(dsn = paste(rootDir, "burrOak", sep=""), 
                             layer = "burrOakSitesEqArea")  # shapefile created with grts function
burrOakSites84 <- spTransform(x = burrOakSitesPlot, #reproject
                               CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
burrOakSites84@data <- mutate(burrOakSites84@data, 
                               long=coordinates(burrOakSites84)[,1], # add long to @data slot
                               lat=coordinates(burrOakSites84)[,2]) # add lat to @data slot
                               

# write out table of overdraw sites for reference in field
write.table(filter(burrOakSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "burrOak/burrOakOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = burrOakSites84, 
         dsn = paste(rootDir, "burrOak", sep=""), 
         layer = "burrOakSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
burrOakSites84ListByPanel <- split(burrOakSites84, # split preserves class, outputs a list
                                    f= burrOakSites84@data$panel)

# to look at the list (optional):
burrOakSites84ListByPanel[[1]]@data        #should be mainSites
burrOakSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = burrOakSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "burrOak", sep=""), 
         layer = "burrOakSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = burrOakSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "burrOak", sep=""), 
         layer = "burrOakSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=burrOakSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 1) # f is zoom.  Large #, less zoom. tweak for each lake.  
burrOakSat <- get_map(location = bbox,
                       color = "color",
                       source = "google",
                       maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(burrOakSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=burrOakEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(burrOakSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(burrOakSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Burr Oak Reservoir")

ggsave(filename=paste(rootDir, "burrOak/burrOakMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(burrOakSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=burrOakEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(burrOakSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(burrOakSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Burr Oak Reservoir")

ggsave(filename=paste(rootDir, "burrOak/burrOakOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(burrOakSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=burrOakEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=burrOakSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=burrOakSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Burr Oak Reservoir")

ggsave(filename=paste(rootDir, "burrOak/burrOakAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
