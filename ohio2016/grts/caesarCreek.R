# CAESAR CREEK LAKE GRTS DESIGN
# LARGE LAKE (10.5 km^2) WITH ONE MAIN TRIBUTARY ARM, TO THE N.
# TRIBUTARY AREAS MAKE UP ONLY A SMALL PORTION OF THE STUDY AREA

# A STRATIFIED-UNEQUAL PROBABILITY GRTS DESIGN
## UNEQUAL IN AN ATTEMPT TO MAKE SAMPLING EASIER ON THE CREW
## THIS GRTS DESIGN HAS ALSO BEEN UPDATED TO HAVE THE EXPANDED ATTRIBUTE TABLE IN THE
## "EQAREA" VERSION OF THE SITE SHAPE FILE

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
caesarCreekEqArea <- readOGR(dsn = paste(rootDir, "caesarCreek", sep=""), # Could use read.shape from spsurvey package
                           layer = "caesarCreekEqArea")  # shapefile name
plot(caesarCreekEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attCaesarCreek <- read.dbf(filename = paste(rootDir, "caesarCreek/caesarCreekEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
### the unequal probability splits the open water part of the lake into two sections of almost equal area
caesarCreekDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                          seltype="Unequal",
                                          caty.n=c("north" = 5,
                                                   "south" = 5),
                                          over=20),
                      "trib"=list(panel=c(mainSites=5),
                                  seltype="Equal",
                                  over=10))

caesarCreekSitesEqArea <- grts(design=caesarCreekDsgn,
                               DesignID="SU", # SU for stratified, unequal
                               type.frame="area",
                               src.frame="shapefile",
                               in.shape=paste(rootDir, "caesarCreek/caesarCreekEqArea", sep=""),
                               att.frame=attCaesarCreek,
                               stratum="strata",
                               mdcaty="section",
                               shapefile=TRUE,
                               out.shape=paste(rootDir, "caesarCreek/caesarCreekSitesEqArea", sep=""),
                               prjfilename=paste(rootDir, "caesarCreek/caesarCreekEqArea", sep=""))

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
caesarCreekSitesEqArea <- readOGR(dsn = paste(rootDir, "caesarCreek", sep=""), # Could use read.shape from spsurvey package
                            layer = "caesarCreekSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
caesarCreekSitesEqArea@data <- mutate(caesarCreekSitesEqArea@data, 
                                deployDate = "",    # adding all of these colums to the 
                                deployTime = "",    # shape file to be filled in the field
                                chamStTime = "",    # tried to enter them in the order they will be filled
                                chamEndTime = "",
                                bubblingObs = "",                                
                                waterDepth = "",                          
                                Temp_F_S = "",
                                DOPercent_S = "",
                                DO_mg_L_S = "",
                                SpCond_ms_S = "",
                                pH_S = "",
                                ORP_S = "",
                                TurbidNTU_S = "",
                                chla_S = "",
                                Temp_F_D = "",
                                DOPercent_D = "",
                                DO_mg_L_D = "",
                                SpCond_ms_D = "",
                                pH_D = "",
                                ORP_D = "",
                                TurbidNTU_D = "",
                                chla_D = "",
                                AirExtnrs = "",
                                DG_Extnrs = "",
                                H2O_vol = "",
                                HeVol = "",
                                BarPrssr = "",
                                RtrvDate = "",
                                RtrvTime = "",
                                TotTrapVol = "",
                                TrapExtnrs = "",
                                Notes = "",
                                LatSamp = "",
                                LongSamp = ""
)


# re-write this mutated file, will keep the equal area projection
writeOGR(obj = caesarCreekSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "caesarCreek", sep=""), 
         layer = "caesarCreekSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# Print the initial six lines of the survey design
head(caesarCreekSitesEqArea@data)

# Print the survey design summary
summary(caesarCreekSitesEqArea)

plot(caesarCreekEqArea)
points(caesarCreekSitesEqArea$xcoord, caesarCreekSitesEqArea$ycoord)

# MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
caesarCreekEqArea84 <- spTransform(x = caesarCreekEqArea, 
                                 CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = caesarCreekEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "caesarCreek", sep=""), 
         layer = "caesarCreekEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
caesarCreekEqArea84@data$id = rownames(caesarCreekEqArea84@data)
caesarCreekEqArea84.f <- fortify(caesarCreekEqArea84, region="id")  # fortify polygon for ggmap/ggplot
caesarCreekEqArea84.f <- merge(caesarCreekEqArea84.f, caesarCreekEqArea84@data, 
                             by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plottin
caesarCreekSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "caesarCreek", sep=""), 
                              layer = "caesarCreekSitesEqArea")  # shapefile created with grts function
caesarCreekSites84 <- spTransform(x = caesarCreekSitesEqAreaPlot, #reproject
                                CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
caesarCreekSites84@data <- mutate(caesarCreekSites84@data, 
                                long=coordinates(caesarCreekSites84)[,1], # add long to @data slot
                                lat=coordinates(caesarCreekSites84)[,2]) # add lat to @data slot
                               

# write out table of overdraw sites for reference in field
write.table(filter(caesarCreekSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, mdcaty, long, lat),
            file = paste(rootDir, "caesarCreek/caesarCreekOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = caesarCreekSites84, 
         dsn = paste(rootDir, "caesarCreek", sep=""), 
         layer = "caesarCreekSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# separate shapefile for mainSites and OverSamp (overdraw sites)
caesarCreekSites84ListByPanel <- split(caesarCreekSites84, # split preserves class, outputs a list
                                     f= caesarCreekSites84@data$panel)

# to look at the list (optional):
caesarCreekSites84ListByPanel[[1]]@data        #should be mainSites
caesarCreekSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' to disk
writeOGR(obj = caesarCreekSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "caesarCreek", sep=""), 
         layer = "caesarCreekSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' to disk
writeOGR(obj = caesarCreekSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "caesarCreek", sep=""), 
         layer = "caesarCreekSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# VISUALIZE SURVEY DESIGN--------
# Get ggmap
bbox <- make_bbox(data=caesarCreekSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f=0.5) # f is zoom.  Large #, less zoom. tweak for each lake.  
caesarCreekSat <- get_map(location = bbox,
                        color = "color",
                        source = "google",
                        maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
# Island not showing up properly for some reason?
ggmap(caesarCreekSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=caesarCreekEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(caesarCreekSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(caesarCreekSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +     ### "#00BFC4" is blue
  coord_equal() +                                                ### "#F8766D" is pink
  ggtitle("Main Measurement locations for Caesar Creek Lake")

ggsave(filename=paste(rootDir, "caesarCreek/caesarCreekMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(caesarCreekSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=caesarCreekEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(caesarCreekSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(caesarCreekSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Oversample locations for Caesar Creek Lake")

ggsave(filename=paste(rootDir, "caesarCreek/caesarCreekOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(caesarCreekSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=caesarCreekEqArea84.f, aes(long, lat, group=group, fill=section)) +
  scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=caesarCreekSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=caesarCreekSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Caesar Creek Lake")

ggsave(filename=paste(rootDir, "caesarCreek/caesarCreekAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
