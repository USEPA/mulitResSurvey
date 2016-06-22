# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

# ATWOOD LAKE GRTS DESIGN
# SAMPLE LAKE TENTATIVELY FOR WEEK OF JUNE 26TH
# MID-SIZE LAKE (6.1 km^2) WITH ONE MAIN TRIBUTARY ARM, TO THE NE.
# TRIBUTARY AREAS MAKE UP A SIZABLE PORTION OF THE RESERVOIR, SO LETS TRY A
# STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW -- LONG SKINNY LAKE 
## WITH A 25 HORSEPOWER LIMIT MEANS LONG TIME BETWEEN SITES 


## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##  MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED 
##  FOR EACH STRATA AND EACH SECTION, FOR ATWOOD:
##    OPEN WATER MAINSITES = 10 (SAMPLE DENSITY: 2.1 SITES/KM2)
##            SECTION A (north) = 5
##            SECTION B (south) = 5
##    OPEN WATER OVER SAMPLE = 20
##    TRIBUTARY MAIN SITES = 5 (SAMPLE DENSITY: 4.2 SITES/KM2)
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
atwoodEqArea <- readOGR(dsn = paste(rootDir, "atwood", sep=""), # Could use read.shape from spsurvey package
                           layer = "atwoodEqArea")  # shapefile name
plot(atwoodEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attatwood <- read.dbf(filename = paste(rootDir, "atwood/atwoodEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
atwoodDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                          seltype="Unequal",
                                          caty.n=c("north" = 5,
                                                   "south" = 5),
                                          over=20),
                      "trib"=list(panel=c(mainSites=5),
                                  seltype="Equal",
                                  over=10))

atwoodSitesEqArea <- grts(design=atwoodDsgn,
                             DesignID="SU", # SU for stratified, unequal
                             type.frame="area",
                             src.frame="shapefile",
                             in.shape=paste(rootDir, "atwood/atwoodEqArea", sep=""),
                             att.frame=attatwood,
                             stratum="strata",
                             mdcaty="section",
                             shapefile=TRUE,
                             out.shape=paste(rootDir, "atwood/atwoodSitesEqArea", sep=""),
                             prjfilename=paste(rootDir, "atwood/atwoodEqArea", sep=""))

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
atwoodSitesEqArea <- readOGR(dsn = paste(rootDir, "atwood", sep=""), # Could use read.shape from spsurvey package
                                layer = "atwoodSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
atwoodSitesEqArea@data <- mutate(atwoodSitesEqArea@data, 
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
writeOGR(obj = atwoodSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "atwood", sep=""), 
         layer = "atwoodSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(atwoodSitesEqArea@data)

# Print the survey design summary
summary(atwoodSitesEqArea)

plot(atwoodEqArea)
points(atwoodSitesEqArea$xcoord, atwoodSitesEqArea$ycoord)

# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
atwoodEqArea84 <- spTransform(x = atwoodEqArea, 
                                 CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = atwoodEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "atwood", sep=""), 
         layer = "atwoodEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
atwoodEqArea84@data$id = rownames(atwoodEqArea84@data)
atwoodEqArea84.f <- fortify(atwoodEqArea84, region="id")  # fortify polygon for ggmap/ggplot
atwoodEqArea84.f <- merge(atwoodEqArea84.f, atwoodEqArea84@data, 
                             by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plottin
atwoodSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "atwood", sep=""), 
                                    layer = "atwoodSitesEqArea")  # shapefile created with grts function
atwoodSites84 <- spTransform(x = atwoodSitesEqAreaPlot, #reproject
                                CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
atwoodSites84@data <- mutate(atwoodSites84@data, 
                                long=coordinates(atwoodSites84)[,1], # add long to @data slot
                                lat=coordinates(atwoodSites84)[,2]) # add lat to @data slot


# write out table of overdraw sites for reference in field
write.table(filter(atwoodSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, mdcaty, long, lat),
            file = paste(rootDir, "atwood/atwoodOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = atwoodSites84, 
         dsn = paste(rootDir, "atwood", sep=""), 
         layer = "atwoodSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
atwoodSites84ListByPanel <- split(atwoodSites84, # split preserves class, outputs a list
                                     f= atwoodSites84@data$panel)

# to look at the list (optional):
atwoodSites84ListByPanel[[1]]@data        #should be mainSites
atwoodSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = atwoodSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "atwood", sep=""), 
         layer = "atwoodSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = atwoodSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "atwood", sep=""), 
         layer = "atwoodSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=atwoodSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f=0.4) # f is zoom.  Large #, less zoom. tweak for each lake.  
atwoodSat <- get_map(location = bbox,
                        color = "color",
                        source = "google",
                        maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(atwoodSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=atwoodEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(atwoodSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(atwoodSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Atwood Lake")

ggsave(filename=paste(rootDir, "atwood/atwoodMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(atwoodSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=atwoodEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(atwoodSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(atwoodSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Atwood Lake")

ggsave(filename=paste(rootDir, "atwood/atwoodOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(atwoodSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=atwoodEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=atwoodSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=atwoodSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Atwood Lake")

ggsave(filename=paste(rootDir, "atwood/atwoodAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
