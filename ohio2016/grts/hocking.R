# HOCKING COUNTY (LAKE LOGAN) GRTS DESIGN
# SMALL LAKE (1.5 km^2) WITH TRIBUTARIES COMING IN FROM
# NORHT WEST.

# AN UNSTRATIFIED UNEQUAL PROBABILITY GRTS DESIGN
## THE UNEQUAL IS TO MINIMIZE DRIVING REQUIRED TO ACCESS OVERSAMPLE SITES.
## THIS GRTS DESIGN HAS ALSO BEEN UPDATED TO HAVE THE EXPANDED ATTRIBUTE TABLE IN THE
## "EQAREA" VERSION OF THE SITE SHAPE FILE

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
hockingEqArea <- readOGR(dsn = paste(rootDir, "hocking", sep=""), # Could use read.shape from spsurvey package
                          layer = "hockingEqArea")  # shapefile name
plot(hockingEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
atthocking <- read.dbf(filename = paste(rootDir, "hocking/hockingEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 6 (area=2.57 sq km)
hockingDsgn <- list(None = list(panel=c(mainSites=15), # unstratified, therefore 1 list
                                seltype="Unequal",    #unequal probability
                                caty.n=c("west" = 7, # these specs will be approximated in final design
                                         "east" = 8),
                                over=10))


hockingSitesEqArea <- grts(design=hockingDsgn,
                            DesignID="US", # US for unstratified
                            type.frame="area",
                            src.frame="shapefile",
                            in.shape=paste(rootDir, "hocking/hockingEqArea", sep=""),
                            att.frame=atthocking,
                            mdcaty="section",
                            shapefile=TRUE,
                            out.shape=paste(rootDir, "hocking/hockingSitesEqArea", sep=""),
                            prjfilename=paste(rootDir, "hocking/hockingEqArea", sep=""))

# Print the initial six lines of the survey design
head(hockingSitesEqArea@data)

# Print the survey design summary
summary(hockingSitesEqArea)

# Plot sample sites
plot(hockingEqArea)
points(hockingSitesEqArea$xcoord, hockingSitesEqArea$ycoord, 
       col=ifelse(hockingSitesEqArea$panel == "OverSamp", "black", "red"))

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able data fields.
# We want these fields in the Equal Area and WGS84 projections of this shapefile.
# Need to read in shapefile created with grts function, then modify, then write back to disk.
hockingSitesEqArea <- readOGR(dsn = paste(rootDir, "hocking", sep=""), # Could use read.shape from spsurvey package
                               layer = "hockingSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
hockingSitesEqArea@data <- mutate(hockingSitesEqArea@data, 
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
writeOGR(obj = hockingSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "hocking", sep=""), 
         layer = "hockingSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
hockingEqArea84 <- spTransform(x = hockingEqArea, 
                                CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = hockingEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "hocking", sep=""), 
         layer = "hockingEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
hockingEqArea84@data$id = rownames(hockingEqArea84@data)
hockingEqArea84.f <- fortify(hockingEqArea84, region="id")  # fortify polygon for ggmap/ggplot
hockingEqArea84.f <- merge(hockingEqArea84.f, hockingEqArea84@data, 
                            by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
hockingSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "hocking", sep=""), 
                                   layer = "hockingSitesEqArea")  # shapefile created with grts function
hockingSites84 <- spTransform(x = hockingSitesEqAreaPlot, #reproject
                               CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
hockingSites84@data <- mutate(hockingSites84@data, 
                               long=coordinates(hockingSites84)[,1], # add long to @data slot
                               lat=coordinates(hockingSites84)[,2]) # add lat to @data slot


# write out table of overdraw sites for reference in field
write.table(filter(hockingSites84@data, panel == "OverSamp")   %>%
              select(siteID, mdcaty, long, lat) %>%
              arrange(mdcaty, siteID),
            file = paste(rootDir, "hocking/hockingOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = hockingSites84, 
         dsn = paste(rootDir, "hocking", sep=""), 
         layer = "hockingSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
hockingSites84ListByPanel <- split(hockingSites84, # split preserves class, outputs a list
                                    f= hockingSites84@data$panel)

# to look at the list (optional):
hockingSites84ListByPanel[[1]]@data        #should be mainSites
hockingSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = hockingSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "hocking", sep=""), 
         layer = "hockingSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = hockingSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "hocking", sep=""), 
         layer = "hockingSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
# Can't get a reasonable zoom.  If zoomed in too far, ggmap corrupts
# the polygon.  Current zoom level (f=1.2) preserves polygon integrity,
# but is to large to be useful.  Made alternative maps in ArcGIS.
bbox <- make_bbox(data=hockingSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f=0.8) # f is zoom.  Large #, less zoom. tweak for each lake.  
hockingSat <- get_map(bbox,
                       color = "color",
                       source = "google",
                       maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(hockingSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=hockingEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(hockingSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(hockingSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Hocking (aka Logan) Lake")

ggsave(filename=paste(rootDir, "hocking/hockingMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(hockingSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=hockingEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(hockingSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(hockingSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Hocking (aka Logan) Lake")

ggsave(filename=paste(rootDir, "hocking/hockingOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(hockingSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=hockingEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=hockingSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=hockingSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for MJ Kirwan (West Branch)")

ggsave(filename=paste(rootDir, "hocking/hockingAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")