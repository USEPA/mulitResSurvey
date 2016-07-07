# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##  MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED 
##  FOR EACH STRATA AND EACH SECTION, NOMINALLY:
##    OPEN WATER MAINSITES = 10
##            SECTION A (NORTH) = X
##            SECTION B (SOUTH) = 10-X
##    OPEN WATER OVER SAMPLE = 20
##    TRIBUTARY MAIN SITES = 5
##    TRIBUTARY OVER SAMPLE = 10
##  CHANGE THE ZOOM FACTOR ON LINE 179
##  FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME


# LaDue Reservoir
# MID-SIZED LAKE (5.7 km^2) WITH ONE MAIN TRIBUTARY ARM, TO THE W.
# For LaDue: Open Water North: 6
          #  Open Water South: 4
          #  Trib: 5

# A STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL PROBABILITY USED IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW
## THIS GRTS DESIGN HAS ALSO BEEN UPDATED TO HAVE THE EXPANDED ATTRIBUTE TABLE IN THE
## "EQAREA" VERSION OF THE SITE SHAPE FILE

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
laDueEqArea <- readOGR(dsn = paste(rootDir, "laDue", sep=""), # Could use read.shape from spsurvey package
                             layer = "laDueEqArea")  # shapefile name
plot(laDueEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attlaDue <- read.dbf(filename = paste(rootDir, "laDue/laDueEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
laDueDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                            seltype="Unequal",
                                            caty.n=c("north" = 6,
                                                     "south" = 4),
                                            over=20),
                        "trib"=list(panel=c(mainSites=5),
                                    seltype="Equal",
                                    over=10))

laDueSitesEqArea <- grts(design=laDueDsgn,
                               DesignID="SU", # SU for stratified, unequal
                               type.frame="area",
                               src.frame="shapefile",
                               in.shape=paste(rootDir, "laDue/laDueEqArea", sep=""),
                               att.frame=attlaDue,
                               stratum="strata",
                               mdcaty="section",
                               shapefile=TRUE,
                               out.shape=paste(rootDir, "laDue/laDueSitesEqArea", sep=""),
                               prjfilename=paste(rootDir, "laDue/laDueEqArea", sep=""))

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
laDueSitesEqArea <- readOGR(dsn = paste(rootDir, "laDue", sep=""), # Could use read.shape from spsurvey package
                                  layer = "laDueSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
laDueSitesEqArea@data <- mutate(laDueSitesEqArea@data, 
                                      deplyDt = "",    # adding all of these colums to the 
                                      deplyTm = "",    # shape file to be filled in the field
                                      chmStTm = "",  # tried to enter them in the order they will be filled
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
writeOGR(obj = laDueSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "laDue", sep=""), 
         layer = "laDueSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(laDueSitesEqArea@data)

# Print the survey design summary
summary(laDueSitesEqArea)

plot(laDueEqArea)
points(laDueSitesEqArea$xcoord, laDueSitesEqArea$ycoord)

# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
laDueEqArea84 <- spTransform(x = laDueEqArea, 
                                   CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = laDueEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "laDue", sep=""), 
         layer = "laDueEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
laDueEqArea84@data$id = rownames(laDueEqArea84@data)
laDueEqArea84.f <- fortify(laDueEqArea84, region="id")  # fortify polygon for ggmap/ggplot
laDueEqArea84.f <- merge(laDueEqArea84.f, laDueEqArea84@data, 
                               by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plottin
laDueSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "laDue", sep=""), 
                                      layer = "laDueSitesEqArea")  # shapefile created with grts function
laDueSites84 <- spTransform(x = laDueSitesEqAreaPlot, #reproject
                                  CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
laDueSites84@data <- mutate(laDueSites84@data, 
                                  long=coordinates(laDueSites84)[,1], # add long to @data slot
                                  lat=coordinates(laDueSites84)[,2]) # add lat to @data slot


# write out table of overdraw sites for reference in field
write.table(filter(laDueSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, mdcaty, long, lat),
            file = paste(rootDir, "laDue/laDueOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = laDueSites84, 
         dsn = paste(rootDir, "laDue", sep=""), 
         layer = "laDueSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
laDueSites84ListByPanel <- split(laDueSites84, # split preserves class, outputs a list
                                       f= laDueSites84@data$panel)

# to look at the list (optional):
laDueSites84ListByPanel[[1]]@data        #should be mainSites
laDueSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = laDueSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "laDue", sep=""), 
         layer = "laDueSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = laDueSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "laDue", sep=""), 
         layer = "laDueSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=laDueSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f=0.3) # f is zoom.  Large #, less zoom. tweak for each lake.  
laDueSat <- get_map(location = bbox,
                          color = "color",
                          source = "google",
                          maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(laDueSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=laDueEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(laDueSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(laDueSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for LaDue Reservoir")

ggsave(filename=paste(rootDir, "laDue/laDueMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(laDueSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=laDueEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(laDueSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(laDueSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for LaDue Reservoir")

ggsave(filename=paste(rootDir, "laDue/laDueOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(laDueSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=laDueEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=laDueSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=laDueSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for LaDue Reservoir")

ggsave(filename=paste(rootDir, "laDue/laDueAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
