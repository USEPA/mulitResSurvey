# ROCKY FORK LAKE GRTS DESIGN
# SAMPLE LAKE FOR WEEK 2 OF STUDY, JUNE 6 - 10
# MEDIUM-LARGE LAKE (8 km^2) WITH ONE MAIN TRIBUTARY.
# TRIBUTARY AREA IS SMALL, SO LETS TRY AN:
# UNSTRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ/PLOT SHAPEFILE ---------
rockyForkEqArea <- readOGR(dsn = paste(rootDir, "rockyFork", sep=""), # Could use read.shape from spsurvey package
                            layer = "rockyForkEqArea")  # shapefile name
plot(rockyForkEqArea) # visualize polygon



# EXTRACT ATTRIBUTE TABLE -----------
# This is referenced in the GRTS att.frame argument.  If src.frame is
# 'shapefile' and the shapefile was created with the sp2shape function, then the the shapefile 
# is stored on disk and does not exist in environment.  Therefore we need to create an obect in
# the environment that contains the information needed for the att.frame argument of the grts
# function.  IF we could get grts to work while specifying src.frame = 'sp.object', then the 
# spatial object (i.e. shapefile) could be read into environment and we could reference 
# shapefile@data in the att.frame argument.  This worked with the UT_ecoregions demo, but we
# can't get it to work here.  
attRockyFork <- read.dbf(filename = paste(rootDir, "rockyFork/rockyForkEqArea", sep=""))



# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
rockyForkDsgn <- list(None=list(panel=c(mainSites=15), # unstratified, therefore 1 list
                                 seltype="Equal",
                                 over = 20)) # Equal probability, have been using 20 for oversample sites


# Execute survey design  
rockyForkSites <- grts(design=rockyForkDsgn,
                        DesignID="U", # U for unstratified
                        type.frame="area",
                        src.frame="shapefile",
                        in.shape=paste(rootDir, "rockyFork/rockyForkEqArea", sep=""),
                        att.frame=attRockyFork,
                        shapefile=TRUE,
                        prjfilename = paste(rootDir, "rockyFork/rockyForkEqArea", sep=""),
                        out.shape=paste(rootDir, "rockyFork/rockyForkSites", sep=""))

# Print the initial six lines of the survey design ------------
head(rockyForkSites@data)


# Print the survey design summary
summary(rockyForkSites)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
rockyForkEqArea84 <- spTransform(x = rockyForkEqArea, 
                                  CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = rockyForkEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "rockyFork", sep=""), 
         layer = "rockyForkEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
rockyForkEqArea84@data$id = rownames(rockyForkEqArea84@data)
rockyForkEqArea84.f <- fortify(rockyForkEqArea84, region="id")  # fortify polygon for ggmap/ggplot
rockyForkEqArea84.f <- merge(rockyForkEqArea84.f, rockyForkEqArea84@data, 
                              by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
rockyForkSitesPlot <- readOGR(dsn = paste(rootDir, "rockyFork", sep=""), 
                               layer = "rockyForkSites")  # shapefile created with grts function
rockyForkSites84 <- spTransform(x = rockyForkSitesPlot, #reproject
                                 CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
rockyForkSites84@data <- mutate(rockyForkSites84@data, 
                                 long=coordinates(rockyForkSites84)[,1], # add long to @data slot
                                 lat=coordinates(rockyForkSites84)[,2], # add lat to @data slot
                                 deployDate = "",    # adding all of these colums to the 
                                 deployTime = "",    # shape file to be filled in the field
                                 waterDepth = "",    # tried to enter them in the order they will be filled                      
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
                                 BarPrssr = "",
                                 RtrvDate = "",
                                 RtrvTime = "",
                                 TotTrapVol = "",
                                 TrapExtnrs = "",
                                 Notes = ""
)

# write out table of overdraw sites for reference in field
write.table(filter(rockyForkSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "rockyFork/rockyForkOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")


# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = rockyForkSites84, 
         dsn = paste(rootDir, "rockyFork", sep=""), 
         layer = "rockyForkSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
rockyForkSites84ListByPanel <- split(rockyForkSites84, # split preserves class, outputs a list
                                      f= rockyForkSites84@data$panel)
# to look at the list (optional):
rockyForkSites84ListByPanel[[1]]@data        #should be the main sites
rockyForkSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = rockyForkSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "rockyFork", sep=""), 
         layer = "rockyForkSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = rockyForkSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "rockyFork", sep=""), 
         layer = "rockyForkSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(rockyForkEqArea)
points(rockyForkSites$xcoord, rockyForkSites$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=rockyForkSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.5) # f is zoom.  Large #, less zoom. tweak for each lake.  
rockyForkSat <- get_map(location = bbox,
                         color = "color",
                         source = "google",
                         maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(rockyForkSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=rockyForkEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(rockyForkSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(rockyForkSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for Rocky Fork Lake")

ggsave(filename=paste(rootDir, "rockyFork/rockyForkmainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(rockyForkSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=rockyForkEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=rockyForkEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(rockyForkSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(rockyForkSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for Rocky Fork Lake")

ggsave(filename=paste(rootDir, "rockyFork/rockyForkOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# Third map contains all sites
ggmap(rockyForkSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=rockyForkEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=rockyForkSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=rockyForkSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Rocky Fork Lake")

ggsave(filename=paste(rootDir, "rockyFork/rockyForkAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

