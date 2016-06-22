# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

# CARR CREEK LAKE GRTS DESIGN
# SMALL LAKE (2 km^2) WITH ONE MAIN TRIBUTARY ARM, TO THE NE.
# TRIBUTARY AREA IS RELATIVELY SMALL, USING STRATIFIED DESIGN
# UNEQUAL IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW


## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##  MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED 
##  FOR EACH STRATA AND EACH SECTION, FOR carr:
##    OPEN WATER MAINSITES = 7
##            SECTION A (MIDDLE) = 2  # number based on trial and error to get desired distribution
##            SECTION B (WEST) = 5 # number based on trial and error to get desired distribution
##    OPEN WATER OVER SAMPLE = 20
##    TRIBUTARY MAIN SITES = 8
##            SECTION A (EAST) = 5 # number based on trial and error to get desired distribution
##            SECTION B (NORTH) = 3 # number based on trial and error to get desired distribution
##    TRIBUTARY OVER SAMPLE = 20
##  CHANGE THE ZOOM FACTOR ON LINE 179
##  FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME


# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
carrCreekEqArea <- readOGR(dsn = paste(rootDir, "carrCreek", sep=""), # Could use read.shape from spsurvey package
                           layer = "carrCreekEqArea")  # shapefile name
plot(carrCreekEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attcarrCreek <- read.dbf(filename = paste(rootDir, "carrCreek/carrCreekEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
carrCreekDsgn <- list("open_water" = list(panel=c(mainSites=7),
                                          seltype="Unequal",
                                          caty.n=c("west" = 5,
                                                   "middle" = 2),
                                          over=10),
                      "trib"=list(panel=c(mainSites=8),
                                  seltype="Unequal",
                                  caty.n=c("east" = 5,
                                           "north" = 3),
                                  over=15))

carrCreekSitesEqArea <- grts(design=carrCreekDsgn,
                             DesignID="SU", # SU for stratified, unequal
                             type.frame="area",
                             src.frame="shapefile",
                             in.shape=paste(rootDir, "carrCreek/carrCreekEqArea", sep=""),
                             att.frame=attcarrCreek,
                             stratum="strata",
                             mdcaty="section",
                             shapefile=TRUE,
                             out.shape=paste(rootDir, "carrCreek/carrCreekSitesEqArea", sep=""),
                             prjfilename=paste(rootDir, "carrCreek/carrCreekEqArea", sep=""))

# Print the initial six lines of the survey design
head(carrCreekSitesEqArea@data)

# Print the survey design summary
summary(carrCreekSitesEqArea)

plot(carrCreekEqArea)
points(carrCreekSitesEqArea$xcoord, carrCreekSitesEqArea$ycoord)

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
carrCreekSitesEqArea <- readOGR(dsn = paste(rootDir, "carrCreek", sep=""), # Could use read.shape from spsurvey package
                                layer = "carrCreekSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
carrCreekSitesEqArea@data <- mutate(carrCreekSitesEqArea@data, 
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
writeOGR(obj = carrCreekSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "carrCreek", sep=""), 
         layer = "carrCreekSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
carrCreekEqArea84 <- spTransform(x = carrCreekEqArea, 
                                 CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = carrCreekEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "carrCreek", sep=""), 
         layer = "carrCreekEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
carrCreekEqArea84@data$id = rownames(carrCreekEqArea84@data)
carrCreekEqArea84.f <- fortify(carrCreekEqArea84, region="id")  # fortify polygon for ggmap/ggplot
carrCreekEqArea84.f <- merge(carrCreekEqArea84.f, carrCreekEqArea84@data, 
                             by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plottin
carrCreekSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "carrCreek", sep=""), 
                                    layer = "carrCreekSitesEqArea")  # shapefile created with grts function
carrCreekSites84 <- spTransform(x = carrCreekSitesEqAreaPlot, #reproject
                                CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
carrCreekSites84@data <- mutate(carrCreekSites84@data, 
                                long=coordinates(carrCreekSites84)[,1], # add long to @data slot
                                lat=coordinates(carrCreekSites84)[,2]) # add lat to @data slot


# write out table of overdraw sites for reference in field
write.table(filter(carrCreekSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, mdcaty, long, lat) %>%
              arrange(stratum, mdcaty, siteID),
            file = paste(rootDir, "carrCreek/carrCreekOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = carrCreekSites84, 
         dsn = paste(rootDir, "carrCreek", sep=""), 
         layer = "carrCreekSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
carrCreekSites84ListByPanel <- split(carrCreekSites84, # split preserves class, outputs a list
                                     f= carrCreekSites84@data$panel)

# to look at the list (optional):
carrCreekSites84ListByPanel[[1]]@data        #should be mainSites
carrCreekSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = carrCreekSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "carrCreek", sep=""), 
         layer = "carrCreekSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = carrCreekSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "carrCreek", sep=""), 
         layer = "carrCreekSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=carrCreekSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f=0.5) # f is zoom.  Large #, less zoom. tweak for each lake.  
carrCreekSat <- get_map(location = bbox,
                        color = "color",
                        source = "google",
                        maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(carrCreekSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=carrCreekEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666", "#003300")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(carrCreekSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(carrCreekSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for carr Creek Lake")

ggsave(filename=paste(rootDir, "carrCreek/carrCreekMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(carrCreekSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=carrCreekEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666", "#003300")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(carrCreekSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(carrCreekSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for carr Creek Lake")

ggsave(filename=paste(rootDir, "carrCreek/carrCreekOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(carrCreekSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=carrCreekEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=carrCreekSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=carrCreekSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for carr Creek Lake")

ggsave(filename=paste(rootDir, "carrCreek/carrCreekAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
