

# AN UNSTRATIFIED UNEQUAL PROBABILITY GRTS DESIGN
## THIS GRTS DESIGN HAS ALSO BEEN UPDATED TO HAVE THE EXPANDED ATTRIBUTE TABLE IN THE
## "EQAREA" VERSION OF THE SITE SHAPE FILE

## CHARLES MILL LAKE IS A MID-SIZE (5.3 KM2), TORTUOUS LAKE
## IT IS UNUSUAL IN THAT IT CAN BE DIVIDED INTO THREE BASINS THAT ARE ONLY ATTACHED VIA NARROW STRAITS
## TWO OF THESE BASINS, THE NW AND SW, ARE VERY SHALLOW (MOSTLY 3-6', SOME FROM 6-9')
### THESE SHALLOW BASINS SHOW SIGNS OF SUBSTANTIAL SILTATION
## THE THIRD BASIN TO THE EAST HAS TWO DEEP HOLES AND IN GENERAL IS ROCKIER

## BECAUSE OF THIS DIFFERENCE IN BASINS WITH NO CLEAR 'TRIBUTARY' REGION, WE CHOSE 
## TO USE AN UNSTRATIFIED, UNEQUAL PROBABILITY DESIGN, WITH THE EAST AND WEST BASINS SEPARATED 
## INTO UNEQUAL SECTIONS, "EAST" HAS AN AREA OF 
##    panel=c(mainSites=15), # unstratified, therefore 1 list
##    seltype="Unequal",    #unequal probability
##    caty.n=c("west" = 5, # 
##         "east" = 10),
##    over=10))


# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
charlesMillEqArea <- readOGR(dsn = paste(rootDir, "charlesMill", sep=""), # Could use read.shape from spsurvey package
                         layer = "charlesMillEqArea")  # shapefile name
plot(charlesMillEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attcharlesMill <- read.dbf(filename = paste(rootDir, "charlesMill/charlesMillEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 6 (area=2.57 sq km)
charlesMillDsgn <- list(None = list(panel=c(mainSites=15), # unstratified, therefore 1 list
                                seltype="Unequal",    #unequal probability
                                caty.n=c("west" = 12, # these specs will be approximated in final design
                                         "east" = 3),
                                over=30))


charlesMillSitesEqArea <- grts(design=charlesMillDsgn,
                           DesignID="US", # US for unstratified
                           type.frame="area",
                           src.frame="shapefile",
                           in.shape=paste(rootDir, "charlesMill/charlesMillEqArea", sep=""),
                           att.frame=attcharlesMill,
                           mdcaty="section",
                           shapefile=TRUE,
                           out.shape=paste(rootDir, "charlesMill/charlesMillSitesEqArea", sep=""),
                           prjfilename=paste(rootDir, "charlesMill/charlesMillEqArea", sep=""))

# Print the initial six lines of the survey design
head(charlesMillSitesEqArea@data)

# Print the survey design summary
summary(charlesMillSitesEqArea)

# Plot sample sites
plot(charlesMillEqArea)
points(charlesMillSitesEqArea$xcoord, charlesMillSitesEqArea$ycoord, 
       col=ifelse(charlesMillSitesEqArea$panel == "OverSamp", "black", "red"))

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able data fields.
# We want these fields in the Equal Area and WGS84 projections of this shapefile.
# Need to read in shapefile created with grts function, then modify, then write back to disk.
charlesMillSitesEqArea <- readOGR(dsn = paste(rootDir, "charlesMill", sep=""), # Could use read.shape from spsurvey package
                              layer = "charlesMillSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
charlesMillSitesEqArea@data <- mutate(charlesMillSitesEqArea@data, 
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
writeOGR(obj = charlesMillSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "charlesMill", sep=""), 
         layer = "charlesMillSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
charlesMillEqArea84 <- spTransform(x = charlesMillEqArea, 
                               CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = charlesMillEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "charlesMill", sep=""), 
         layer = "charlesMillEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
charlesMillEqArea84@data$id = rownames(charlesMillEqArea84@data)
charlesMillEqArea84.f <- fortify(charlesMillEqArea84, region="id")  # fortify polygon for ggmap/ggplot
charlesMillEqArea84.f <- merge(charlesMillEqArea84.f, charlesMillEqArea84@data, 
                           by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
charlesMillSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "charlesMill", sep=""), 
                                  layer = "charlesMillSitesEqArea")  # shapefile created with grts function
charlesMillSites84 <- spTransform(x = charlesMillSitesEqAreaPlot, #reproject
                              CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
charlesMillSites84@data <- mutate(charlesMillSites84@data, 
                              long=coordinates(charlesMillSites84)[,1], # add long to @data slot
                              lat=coordinates(charlesMillSites84)[,2]) # add lat to @data slot


# write out table of overdraw sites for reference in field
write.table(filter(charlesMillSites84@data, panel == "OverSamp")   %>%
              select(siteID, mdcaty, long, lat) %>%
              arrange(mdcaty, siteID),
            file = paste(rootDir, "charlesMill/charlesMillOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = charlesMillSites84, 
         dsn = paste(rootDir, "charlesMill", sep=""), 
         layer = "charlesMillSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
charlesMillSites84ListByPanel <- split(charlesMillSites84, # split preserves class, outputs a list
                                   f= charlesMillSites84@data$panel)

# to look at the list (optional):
charlesMillSites84ListByPanel[[1]]@data        #should be mainSites
charlesMillSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = charlesMillSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "charlesMill", sep=""), 
         layer = "charlesMillSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = charlesMillSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "charlesMill", sep=""), 
         layer = "charlesMillSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
# Can't get a reasonable zoom.  If zoomed in too far, ggmap corrupts
# the polygon.  Current zoom level (f=1.2) preserves polygon integrity,
# but is to large to be useful.  Made alternative maps in ArcGIS.
bbox <- make_bbox(data=charlesMillSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f=0.9) # f is zoom.  Large #, less zoom. tweak for each lake.  
charlesMillSat <- get_map(bbox,
                      color = "color",
                      source = "google",
                      maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(charlesMillSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=charlesMillEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(charlesMillSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(charlesMillSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Charles Mill Lake")

ggsave(filename=paste(rootDir, "charlesMill/charlesMillMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(charlesMillSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=charlesMillEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(charlesMillSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(charlesMillSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Charles Mill Lake")

ggsave(filename=paste(rootDir, "charlesMill/charlesMillOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(charlesMillSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=charlesMillEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=charlesMillSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=charlesMillSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Charles Mill Lake")

ggsave(filename=paste(rootDir, "charlesMill/charlesMillAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
