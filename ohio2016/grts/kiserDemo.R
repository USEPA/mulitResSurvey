# DEMO:  KISER LAKE DESIGN
# SHALLOW LAKE WITH NO OBVIOUS 'TRIBUTARY AREA'.
# WILL USE AN UNSTRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ/PLOT SHAPEFILE ---------
  kiserLakeEqArea <- readOGR(dsn = paste(rootDir, "kiserLakeDemo", sep=""), # Could use read.shape from spsurvey package
                      layer = "kiserLakeEqArea")  # shapefile name
    plot(kiserLakeEqArea) # visualize polygon
  
# CREATE SHAPEFILE -- no longer using ---------------
# We read in a shapefile, but apparently the sp2shape
# function is needed to prep this object for the grts function.  Also
# tried referencing the object as a 'sp.object' in the grts argument 'src.frame',
# but that failed to (see below).  The approach defined below works, but must specify projection 
# in sp2shape function AND in the grts call.  Seem that we should be able to use
# the readOGR object directly, but can't get it to work.
  
#sp2shape(sp.obj=kiserLakeEqArea, # object created with readOGR or read.shape
 #        prjfilename = paste(rootDir, "kiserLakeDemo/kiserLakeEqArea", sep=""), # proj from original shapefile
  #       shpfilename=paste(rootDir, "kiserLakeDemo/kiserLakeEqAreaShp", sep="")) # name of created shapefile.

# EXTRACT ATTRIBUTE TABLE -----------
# This is referenced in the GRTS att.frame argument.  If src.frame is
# 'shapefile' and the shapefile was created with the sp2shape function, then the the shapefile 
# is stored on disk and does not exist in environment.  Therefore we need to create an obect in
# the environment that contains the information needed for the att.frame argument of the grts
# function.  IF we could get grts to work while specifying src.frame = 'sp.object', then the 
# spatial object (i.e. shapefile) could be read into environment and we could reference 
# shapefile@data in the att.frame argument.  This worked with the UT_ecoregions demo, but we
# can't get it to work here.  
attKiser <- read.dbf(filename = paste(rootDir, "kiserLakeDemo/kiserLakeEqArea", sep=""))

  

# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
kiserDsgn <- list(None=list(panel=c(mainSites=15), # unstratified, therefore 1 list
                            seltype="Equal",
                            overSites = 20)) # Equal probability, have been using 20 for oversample sites


# Execute survey design  
kiserSites <- grts(design=kiserDsgn,
                   DesignID="U", # U for unstratified
                   type.frame="area",
                   src.frame="shapefile",
                   in.shape=paste(rootDir, "kiserLakeDemo/kiserLakeEqArea", sep=""),
                   att.frame=attKiser,
                   shapefile=TRUE,
                   prjfilename = paste(rootDir, "kiserLakeDemo/kiserLakeEqArea", sep=""),
                   out.shape=paste(rootDir, "kiserLakeDemo/kiserSites", sep=""))

# Print the initial six lines of the survey design ------------
head(kiserSites@data)

# Print the survey design summary
summary(kiserSites)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
kiserEqArea84 <- spTransform(x = kiserLakeEqArea, 
                                   CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = kiserEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
         layer = "kiserEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
kiserEqArea84@data$id = rownames(kiserEqArea84@data)
kiserEqArea84.f <- fortify(kiserEqArea84, region="id")  # fortify polygon for ggmap/ggplot
kiserEqArea84.f <- merge(kiserEqArea84.f, kiserEqArea84@data, 
                               by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
kiserSitesPlot <- readOGR(dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
                                layer = "kiserSites")  # shapefile created with grts function
kiserSites84 <- spTransform(x = kiserSitesPlot, #reproject
                                  CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
kiserSites84@data <- mutate(kiserSites84@data, 
                            long=coordinates(kiserSites84)[,1], # add long to @data slot
                            lat=coordinates(kiserSites84)[,2], # add lat to @data slot
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
write.table(filter(kiserSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "kiserLakeDemo/kiserOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")

# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = kiserSites84, 
         dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
         layer = "kiserSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for panelOne (main sites) and OverSamp (overdraw sites)
kiserSites84ListByPanel <- split(kiserSites84, # split preserves class, outputs a list
                                       f= kiserSites84@data$panel)
# to look at the list (optional):
kiserSites84ListByPanel[[1]]@data   # should be main sites
kiserSites84ListByPanel[[2]]@data   # should be over sample

# Write 'OverSamp' to disk
writeOGR(obj = kiserSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
         layer = "kiserSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'PanelOne' to disk
writeOGR(obj = kiserSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
         layer = "kiserSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(kiserLakeEqArea)
points(kiserSites$xcoord, kiserSites$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=kiserSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.25) # f is zoom.  Large #, less zoom. tweak for each lake.  
kiserSat <- get_map(location = bbox,
                          color = "color",
                          source = "google",
                          maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(kiserSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=kiserEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(kiserSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(kiserSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") + #00BFC4 other color
  coord_equal() +
  ggtitle("Main Measurement locations for Kiser Lake")

ggsave(filename=paste(rootDir, "kiserLakeDemo/kiserMainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(kiserSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=kiserEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=kiserEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(kiserSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(kiserSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +   #F8766D
  coord_equal() +
  ggtitle("Oversample locations for Kiser Lake")

ggsave(filename=paste(rootDir, "kiserLakeDemo/kiserOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(kiserSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=kiserEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=kiserSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=kiserSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Main Sites", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Kiser Lake")

ggsave(filename=paste(rootDir, "kiserLakeDemo/kiserAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")




