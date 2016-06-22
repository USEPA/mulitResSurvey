# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

# TAPPAN LAKE GRTS DESIGN
# SAMPLE LAKE FOR WEEK OF JUNE 26 - 30
# LARGE LAKE (8.6 km^2) WITH TWO TRIBUTARY ARMS, TO THE SE.
# TRIBUTARY AREAS MAKE UP A SIZABLE PORTION OF THE RESERVOIR, SO LETS TRY A
# STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW


## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##  MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED 
##  FOR EACH STRATA AND EACH SECTION, FOR TAPPAN:
##    OPEN WATER MAINSITES = 9
##            SECTION A (NORTH) = 5
##            SECTION B (SOUTH) = 4
##    OPEN WATER OVER SAMPLE = 20
##    TRIBUTARY MAIN SITES = 6
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
tappanEqArea <- readOGR(dsn = paste(rootDir, "tappan", sep=""), # Could use read.shape from spsurvey package
                           layer = "tappanEqArea")  # shapefile name
plot(tappanEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
atttappan <- read.dbf(filename = paste(rootDir, "tappan/tappanEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
tappanDsgn <- list("open_water" = list(panel=c(mainSites=9),
                                          seltype="Unequal",
                                          caty.n=c("north" = 5,
                                                   "south" = 4),
                                          over=18),
                      "trib"=list(panel=c(mainSites=6),
                                  seltype="Equal",
                                  over=10))

tappanSitesEqArea <- grts(design=tappanDsgn,
                             DesignID="SU", # SU for stratified, unequal
                             type.frame="area",
                             src.frame="shapefile",
                             in.shape=paste(rootDir, "tappan/tappanEqArea", sep=""),
                             att.frame=atttappan,
                             stratum="strata",
                             mdcaty="section",
                             shapefile=TRUE,
                             out.shape=paste(rootDir, "tappan/tappanSitesEqArea", sep=""),
                             prjfilename=paste(rootDir, "tappan/tappanEqArea", sep=""))

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
tappanSitesEqArea <- readOGR(dsn = paste(rootDir, "tappan", sep=""), # Could use read.shape from spsurvey package
                                layer = "tappanSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
tappanSitesEqArea@data <- mutate(tappanSitesEqArea@data, 
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
writeOGR(obj = tappanSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "tappan", sep=""), 
         layer = "tappanSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(tappanSitesEqArea@data)

# Print the survey design summary
summary(tappanSitesEqArea)

plot(tappanEqArea)
points(tappanSitesEqArea$xcoord, tappanSitesEqArea$ycoord)

# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
tappanEqArea84 <- spTransform(x = tappanEqArea, 
                                 CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = tappanEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "tappan", sep=""), 
         layer = "tappanEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
tappanEqArea84@data$id = rownames(tappanEqArea84@data)
tappanEqArea84.f <- fortify(tappanEqArea84, region="id")  # fortify polygon for ggmap/ggplot
tappanEqArea84.f <- merge(tappanEqArea84.f, tappanEqArea84@data, 
                             by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plottin
tappanSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "tappan", sep=""), 
                                    layer = "tappanSitesEqArea")  # shapefile created with grts function
tappanSites84 <- spTransform(x = tappanSitesEqAreaPlot, #reproject
                                CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
tappanSites84@data <- mutate(tappanSites84@data, 
                                long=coordinates(tappanSites84)[,1], # add long to @data slot
                                lat=coordinates(tappanSites84)[,2]) # add lat to @data slot


# write out table of overdraw sites for reference in field
write.table(filter(tappanSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, mdcaty, long, lat),
            file = paste(rootDir, "tappan/tappanOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = tappanSites84, 
         dsn = paste(rootDir, "tappan", sep=""), 
         layer = "tappanSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
tappanSites84ListByPanel <- split(tappanSites84, # split preserves class, outputs a list
                                     f= tappanSites84@data$panel)

# to look at the list (optional):
tappanSites84ListByPanel[[1]]@data        #should be mainSites
tappanSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = tappanSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "tappan", sep=""), 
         layer = "tappanSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = tappanSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "tappan", sep=""), 
         layer = "tappanSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=tappanSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f=0.5) # f is zoom.  Large #, less zoom. tweak for each lake.  
tappanSat <- get_map(location = bbox,
                        color = "color",
                        source = "google",
                        maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(tappanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=tappanEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(tappanSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(tappanSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Tappan Lake")

ggsave(filename=paste(rootDir, "tappan/tappanMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(tappanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=tappanEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(tappanSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(tappanSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Tappan Lake")

ggsave(filename=paste(rootDir, "tappan/tappanOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(tappanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=tappanEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=tappanSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=tappanSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Tappan Lake")

ggsave(filename=paste(rootDir, "tappan/tappanAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
