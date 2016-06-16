# MJ KIRWAN LAKE GRTS DESIGN
# LARGE LAKE (10.5 km^2) WITH LARGEST TRIBUTARY ARM COMING IN FROM
# WEST.  ANOTHER LARGE TRIB COMES INTO NORTH SHORE, ALSO ON WEST SIDE
# OF LAKE.

# A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN
## THIS GRTS DESIGN HAS ALSO BEEN UPDATED TO HAVE THE EXPANDED ATTRIBUTE TABLE IN THE
## "EQAREA" VERSION OF THE SITE SHAPE FILE

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
mjKirwanEqArea <- readOGR(dsn = paste(rootDir, "mjKirwan", sep=""), # Could use read.shape from spsurvey package
                             layer = "mjKirwanEqArea")  # shapefile name
plot(mjKirwanEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attmjKirwan <- read.dbf(filename = paste(rootDir, "mjKirwan/mjKirwanEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 6 (area=2.57 sq km)
mjKirwanDsgn <- list("open_water" = list(panel=c(mainSites=9),
                                         seltype="Equal",
                                         over=10),
                     "trib"=list(panel=c(mainSites=6),
                                 seltype="Equal",
                                 over=10))


mjKirwanSitesEqArea <- grts(design=mjKirwanDsgn,
                               DesignID="S", # S for stratified
                               type.frame="area",
                               src.frame="shapefile",
                               in.shape=paste(rootDir, "mjKirwan/mjKirwanEqArea", sep=""),
                               att.frame=attmjKirwan,
                               stratum="strata",
                               shapefile=TRUE,
                               out.shape=paste(rootDir, "mjKirwan/mjKirwanSitesEqArea", sep=""),
                               prjfilename=paste(rootDir, "mjKirwan/mjKirwanEqArea", sep=""))

# Print the initial six lines of the survey design
head(mjKirwanSitesEqArea@data)

# Print the survey design summary
summary(mjKirwanSitesEqArea)

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able data fields.
# We want these fields in the Equal Area and WGS84 projections of this shapefile.
# Need to read in shapefile created with grts function, then modify, then write back to disk.
mjKirwanSitesEqArea <- readOGR(dsn = paste(rootDir, "mjKirwan", sep=""), # Could use read.shape from spsurvey package
                                  layer = "mjKirwanSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
mjKirwanSitesEqArea@data <- mutate(mjKirwanSitesEqArea@data, 
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
writeOGR(obj = mjKirwanSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "mjKirwan", sep=""), 
         layer = "mjKirwanSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)



plot(mjKirwanEqArea)
points(mjKirwanSitesEqArea$xcoord, mjKirwanSitesEqArea$ycoord)

# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
mjKirwanEqArea84 <- spTransform(x = mjKirwanEqArea, 
                                   CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = mjKirwanEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "mjKirwan", sep=""), 
         layer = "mjKirwanEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
mjKirwanEqArea84@data$id = rownames(mjKirwanEqArea84@data)
mjKirwanEqArea84.f <- fortify(mjKirwanEqArea84, region="id")  # fortify polygon for ggmap/ggplot
mjKirwanEqArea84.f <- merge(mjKirwanEqArea84.f, mjKirwanEqArea84@data, 
                               by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
mjKirwanSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "mjKirwan", sep=""), 
                                      layer = "mjKirwanSitesEqArea")  # shapefile created with grts function
mjKirwanSites84 <- spTransform(x = mjKirwanSitesEqAreaPlot, #reproject
                                  CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
mjKirwanSites84@data <- mutate(mjKirwanSites84@data, 
                                  long=coordinates(mjKirwanSites84)[,1], # add long to @data slot
                                  lat=coordinates(mjKirwanSites84)[,2]) # add lat to @data slot


# write out table of overdraw sites for reference in field
write.table(filter(mjKirwanSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, mdcaty, long, lat),
            file = paste(rootDir, "mjKirwan/mjKirwanOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = mjKirwanSites84, 
         dsn = paste(rootDir, "mjKirwan", sep=""), 
         layer = "mjKirwanSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
mjKirwanSites84ListByPanel <- split(mjKirwanSites84, # split preserves class, outputs a list
                                       f= mjKirwanSites84@data$panel)

# to look at the list (optional):
mjKirwanSites84ListByPanel[[1]]@data        #should be mainSites
mjKirwanSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = mjKirwanSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "mjKirwan", sep=""), 
         layer = "mjKirwanSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = mjKirwanSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "mjKirwan", sep=""), 
         layer = "mjKirwanSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
# Can't get a reasonable zoom.  If zoomed in too far, ggmap corrupts
# the polygon.  Current zoom level (f=1.2) preserves polygon integrity,
# but is to large to be useful.  Made alternative maps in ArcGIS.
bbox <- make_bbox(data=mjKirwanSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f=1.2) # f is zoom.  Large #, less zoom. tweak for each lake.  
mjKirwanSat <- get_map(bbox,
                       color = "color",
                       source = "google",
                       maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(mjKirwanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=mjKirwanEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(mjKirwanSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(mjKirwanSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for MJ Kirwan (West Branch) Lake")

ggsave(filename=paste(rootDir, "mjKirwan/mjKirwanMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(mjKirwanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=mjKirwanEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(mjKirwanSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(mjKirwanSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for MJ Kirwan (West Branch)")

ggsave(filename=paste(rootDir, "mjKirwan/mjKirwanOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(mjKirwanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=mjKirwanEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=mjKirwanSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=mjKirwanSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for MJ Kirwan (West Branch)")

ggsave(filename=paste(rootDir, "mjKirwan/mjKirwanAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
