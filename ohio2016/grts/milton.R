# STRATIFIED, UNEQUAL PROBABILITY GRTS DESIGN

# MILTON LAKE GRTS DESIGN
# MID-SIZE LAKE (6.7 km^2) WITH ONE MAIN TRIBUTARY ARM, TO THE S.
# TRIBUTARY AREAS MAKE UP A SIZABLE PORTION OF THE RESERVOIR, SO LETS TRY A
# STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW
## MIGHT BE WAKE LIMIT ON SOUTHERN END OF LAKE, BUT NORTHERN END
## SHOULD BE UNLIMITED SPEED.

## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##  MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED 
##  FOR EACH STRATA AND EACH SECTION, FOR milton:
##    OPEN WATER MAINSITES = 10
##            SECTION A (NORTH) = 5
##            SECTION B (SOUTH) = 5
##    OPEN WATER OVER SAMPLE = 20
##    TRIBUTARY MAIN SITES = 5
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
miltonEqArea <- readOGR(dsn = paste(rootDir, "milton", sep=""), # Could use read.shape from spsurvey package
                          layer = "miltonEqArea")  # shapefile name
plot(miltonEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attmilton <- read.dbf(filename = paste(rootDir, "milton/miltonEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
miltonDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                         seltype="Unequal",
                                         caty.n=c("north" = 6,
                                                  "south" = 4),
                                         over=20),
                     "trib"=list(panel=c(mainSites=5),
                                 seltype="Equal",
                                 over=15))

miltonSitesEqArea <- grts(design=miltonDsgn,
                            DesignID="SU", # SU for stratified, unequal
                            type.frame="area",
                            src.frame="shapefile",
                            in.shape=paste(rootDir, "milton/miltonEqArea", sep=""),
                            att.frame=attmilton,
                            stratum="strata",
                            mdcaty="section",
                            shapefile=TRUE,
                            out.shape=paste(rootDir, "milton/miltonSitesEqArea", sep=""),
                            prjfilename=paste(rootDir, "milton/miltonEqArea", sep=""))

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
miltonSitesEqArea <- readOGR(dsn = paste(rootDir, "milton", sep=""), # Could use read.shape from spsurvey package
                               layer = "miltonSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
miltonSitesEqArea@data <- mutate(miltonSitesEqArea@data, 
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
writeOGR(obj = miltonSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "milton", sep=""), 
         layer = "miltonSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(miltonSitesEqArea@data)

# Print the survey design summary
summary(miltonSitesEqArea)

plot(miltonEqArea)
points(miltonSitesEqArea$xcoord, miltonSitesEqArea$ycoord)

# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
miltonEqArea84 <- spTransform(x = miltonEqArea, 
                                CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = miltonEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "milton", sep=""), 
         layer = "miltonEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
miltonEqArea84@data$id = rownames(miltonEqArea84@data)
miltonEqArea84.f <- fortify(miltonEqArea84, region="id")  # fortify polygon for ggmap/ggplot
miltonEqArea84.f <- merge(miltonEqArea84.f, miltonEqArea84@data, 
                            by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plottin
miltonSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "milton", sep=""), 
                                   layer = "miltonSitesEqArea")  # shapefile created with grts function
miltonSites84 <- spTransform(x = miltonSitesEqAreaPlot, #reproject
                               CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
miltonSites84@data <- mutate(miltonSites84@data, 
                               long=coordinates(miltonSites84)[,1], # add long to @data slot
                               lat=coordinates(miltonSites84)[,2]) # add lat to @data slot


# write out table of overdraw sites for reference in field
write.table(filter(miltonSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, mdcaty, long, lat) %>%
              arrange(stratum, mdcaty, siteID),
            file = paste(rootDir, "milton/miltonOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = miltonSites84, 
         dsn = paste(rootDir, "milton", sep=""), 
         layer = "miltonSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
miltonSites84ListByPanel <- split(miltonSites84, # split preserves class, outputs a list
                                    f= miltonSites84@data$panel)

# to look at the list (optional):
miltonSites84ListByPanel[[1]]@data        #should be mainSites
miltonSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = miltonSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "milton", sep=""), 
         layer = "miltonSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = miltonSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "milton", sep=""), 
         layer = "miltonSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=miltonSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f=2) # f is zoom.  Large #, less zoom. tweak for each lake.  
miltonSat <- get_map(location = bbox,
                       color = "color",
                       source = "google",
                       maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(miltonSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=miltonEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(miltonSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(miltonSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Milton Lake")

ggsave(filename=paste(rootDir, "milton/miltonMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(miltonSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=miltonEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(miltonSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(miltonSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Milton Lake")

ggsave(filename=paste(rootDir, "milton/miltonOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(miltonSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=miltonEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=miltonSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=miltonSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for milton Lake")

ggsave(filename=paste(rootDir, "milton/miltonAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
