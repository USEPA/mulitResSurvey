# BROOKEVILLE LAKE DESIGN

# STRATIFIED UNEQUAL-PROBABILITY
# LARGE LAKE (20.69 km2), ONE MAJOR TRIB

## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##  MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED 
##  FOR EACH STRATA AND EACH SECTION, FOR BROOKEVILLE:
##    OPEN WATER MAINSITES = 8
##            SECTION A (NORTH) = 4
##            SECTION B (SOUTH) = 4
##    OPEN WATER OVER SAMPLE = 20
##    TRIBUTARY MAIN SITES = 7
##    TRIBUTARY OVER SAMPLE = 10
##  CHANGE THE ZOOM FACTOR ON LINE 179
##  FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME


# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
brookevilleEqArea <- readOGR(dsn = paste(rootDir, "brookeville", sep=""), # Could use read.shape from spsurvey package
                         layer = "brookevilleEqArea")  # shapefile name
plot(brookevilleEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attbrookeville <- read.dbf(filename = paste(rootDir, "brookeville/brookevilleEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
brookevilleDsgn <- list("open_water" = list(panel=c(mainSites=15),
                                        seltype="Unequal",
                                        caty.n=c("north" = 8,
                                                 "south" = 7),
                                        over=10),
                    "trib"=list(panel=c(mainSites=5),
                                seltype="Equal",
                                over=10))

brookevilleSitesEqArea <- grts(design=brookevilleDsgn,
                           DesignID="SU", # SU for stratified, unequal
                           type.frame="area",
                           src.frame="shapefile",
                           in.shape=paste(rootDir, "brookeville/brookevilleEqArea", sep=""),
                           att.frame=attbrookeville,
                           stratum="strata",
                           mdcaty="section",
                           shapefile=TRUE,
                           out.shape=paste(rootDir, "brookeville/brookevilleSitesEqArea", sep=""),
                           prjfilename=paste(rootDir, "brookeville/brookevilleEqArea", sep=""))

# Review the survey design
select(brookevilleSitesEqArea@data, mdcaty, stratum, panel) %>%
  arrange(stratum, mdcaty,panel)

# Print the survey design summary
summary(brookevilleSitesEqArea)

plot(brookevilleEqArea)
points(brookevilleSitesEqArea$xcoord, brookevilleSitesEqArea$ycoord)

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
brookevilleSitesEqArea <- readOGR(dsn = paste(rootDir, "brookeville", sep=""), # Could use read.shape from spsurvey package
                              layer = "brookevilleSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
brookevilleSitesEqArea@data <- mutate(brookevilleSitesEqArea@data, 
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
writeOGR(obj = brookevilleSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "brookeville", sep=""), 
         layer = "brookevilleSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
brookevilleEqArea84 <- spTransform(x = brookevilleEqArea, 
                               CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = brookevilleEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "brookeville", sep=""), 
         layer = "brookevilleEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
brookevilleEqArea84@data$id = rownames(brookevilleEqArea84@data)
brookevilleEqArea84.f <- fortify(brookevilleEqArea84, region="id")  # fortify polygon for ggmap/ggplot
brookevilleEqArea84.f <- merge(brookevilleEqArea84.f, brookevilleEqArea84@data, 
                           by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plottin
brookevilleSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "brookeville", sep=""), 
                                  layer = "brookevilleSitesEqArea")  # shapefile created with grts function
brookevilleSites84 <- spTransform(x = brookevilleSitesEqAreaPlot, #reproject
                              CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
brookevilleSites84@data <- mutate(brookevilleSites84@data, 
                              long=coordinates(brookevilleSites84)[,1], # add long to @data slot
                              lat=coordinates(brookevilleSites84)[,2]) # add lat to @data slot


# write out table of overdraw sites for reference in field
write.table(filter(brookevilleSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, mdcaty, long, lat) %>%
              arrange(stratum, mdcaty),
            file = paste(rootDir, "brookeville/brookevilleOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = brookevilleSites84, 
         dsn = paste(rootDir, "brookeville", sep=""), 
         layer = "brookevilleSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
brookevilleSites84ListByPanel <- split(brookevilleSites84, # split preserves class, outputs a list
                                   f= brookevilleSites84@data$panel)

# to look at the list (optional):
brookevilleSites84ListByPanel[[1]]@data        #should be mainSites
brookevilleSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = brookevilleSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "brookeville", sep=""), 
         layer = "brookevilleSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = brookevilleSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "brookeville", sep=""), 
         layer = "brookevilleSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=brookevilleSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f=6.5) # f is zoom.  Large #, less zoom. tweak for each lake.  
brookevilleSat <- get_map(location = bbox,
                      color = "color",
                      source = "google",
                      maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(brookevilleSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=brookevilleEqArea84.f, aes(long, lat, group=group, fill=section)) +
  #scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(brookevilleSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#FFFFFF") + # specify color to be consistent across maps
  geom_text(data=filter(brookevilleSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#FFFFFF") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Brookville Lake")

ggsave(filename=paste(rootDir, "brookeville/brookevilleMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(brookevilleSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=brookevilleEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(brookevilleSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(brookevilleSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Brookville Lake")

ggsave(filename=paste(rootDir, "brookeville/brookevilleOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(brookevilleSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=brookevilleEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=brookevilleSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=brookevilleSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Brookville Lake")

ggsave(filename=paste(rootDir, "brookeville/brookevilleAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")