# HARSHA DEMO 2 -- SECOND SET OF HARSHA GRTS SAMPLE SITES
    #DIFFERENCES ARE 
        # FOLDER IS harshaDemo2
        # setseed is different (in "SET UP AND RUN GRTS FUNCTION section)
        # DesignID is S2
        # Added "2" as a suffix for many of the file names
# DEMO:  HARSHA LAKE DESIGN -- FOR ADAM'S TRAINING RUNS
# WELL STUDIED DEEP LAKE. FOR PURPOSES OF THIS STUDY, WILL STRATIFY BY 
# 'TRIB' AND 'OPEN-WATER'. THE ONE MAIN INLET ARM WILL BE THE ONLY TRIB.
# WILL USE A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
harshaEqArea <- readOGR(dsn = paste(rootDir, "harshaDemo2", sep=""), # Could use read.shape from spsurvey package
                             layer = "harshaEqArea")  # shapefile name
plot(harshaEqArea) # visualize polygon

# CREATE SHAPEFILE -- no longer needed------------------------

#sp2shape(sp.obj=harshaEqArea, # object created with readOGR or read.shape
 #       prjfilename = paste(rootDir, "harshaDemo2/harshaEqArea", sep=""), # proj from original shapefile
  #     shpfilename=paste(rootDir, "harshaDemo2/harshaEqAreaShp", sep="")) # name of created shapefile.

# EXTRACT ATTRIBUTE TABLE -------------------------
attharsha <- read.dbf(filename = paste(rootDir, "harshaDemo2/harshaEqArea", sep=""))


# SET UP AND RUN GRTS FUNCTION ------------------------
# Call the set.seed function so that the survey designs can be replicated
set.seed(444865) #4447864  shoot -- the seven got deleted somewhere in there

# Create the design list
harshaDsgn <- list("open_water" = list(panel=c(PanelOne=10),
                                            seltype="Equal",
                                            over=20),
                        "trib"=list(panel=c(PanelOne=5),
                                    seltype="Equal",
                                    over=20))

harshaSites <- grts(design=harshaDsgn,
                         DesignID="S2", #S for stratified
                         type.frame="area",
                         src.frame="shapefile",
                         in.shape=paste(rootDir, "harshaDemo2/harshaEqArea", sep=""),
                         att.frame=attharsha,
                         stratum="Strata",
                         shapefile=TRUE,
                         out.shape=paste(rootDir, "harshaDemo2/harshaSites", sep=""),
                         prjfilename=paste(rootDir, "harshaDemo2/harshaEqArea", sep=""))


# Print the initial six lines of the survey design
head(harshaSites@data)

# Print the survey design summary
summary(harshaSites)

# NEW: PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
harshaEqArea84 <- spTransform(x = harshaEqArea, 
                                   CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = harshaEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "harshaDemo2", sep=""), 
         layer = "harshaEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
harshaEqArea84@data$id = rownames(harshaEqArea84@data)
harshaEqArea84.f <- fortify(harshaEqArea84, region="id")  # fortify polygon for ggmap/ggplot
harshaEqArea84.f <- merge(harshaEqArea84.f, harshaEqArea84@data, 
                               by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
harshaSitesPlot <- readOGR(dsn = paste(rootDir, "harshaDemo2", sep=""), 
                                layer = "harshaSites")  # shapefile created with grts function
harshaSites84 <- spTransform(x = harshaSitesPlot, #reproject
                                  CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
harshaSites84@data <- mutate(harshaSites84@data, 
                             long=coordinates(harshaSites84)[,1], # add long to @data slot
                             lat=coordinates(harshaSites84)[,2], # add lat to @data slot
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
# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = harshaSites84, 
         dsn = paste(rootDir, "harshaDemo2", sep=""), 
         layer = "harshaSites84_2",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for panelOne (main sites) and OverSamp (overdraw sites)
harshaSites84ListByPanel <- split(harshaSites84, # split preserves class, outputs a list
                                       f= harshaSites84@data$panel)

# to look at the list (optional):
harshaSites84ListByPanel[[1]]@data
harshaSites84ListByPanel[[2]]@data

# Write 'OverSamp' to disk
writeOGR(obj = harshaSites84ListByPanel[[1]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "harshaDemo2", sep=""), 
         layer = "harshaSites84OverSamp2",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'PanelOne' to disk
writeOGR(obj = harshaSites84ListByPanel[[2]], # pulls out 'PanelOne' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "harshaDemo2", sep=""), 
         layer = "harshaSites84PanelOne2",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# visualize survey design with basic R tools -------------------------------
harshaSitesPanelOne <- filter(harshaSites@data, panel=="PanelOne") # extract main sites
harshaSitesOver <- filter(harshaSites@data, panel=="OverSamp") # extract oversample sites
plot(harshaEqArea) #plot polygon
points(harshaSitesPanelOne$xcoord, # plot main sites 
       harshaSitesPanelOne$ycoord, 
       pch=16, col="red", cex=1.5, ) # specify symbol type (circle w/fill), color, and size.
points(harshaSitesOver$xcoord, 
       harshaSitesOver$ycoord, 
       pch=16, col="black") # specify symbol type (circle w/fill), color, and size.

# NEW: VISUALIZE SURVEY DESIGN WITH GGMAP--------
# Get ggmap
bbox <- make_bbox(data=harshaSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.2) # f is zoom.  Large #, less zoom. tweak for each lake.  
harshaSat <- get_map(location = bbox,
                          color = "color",
                          source = "google",
                          maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(harshaSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=harshaEqArea84.f, aes(long, lat, group=group, fill=Strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(harshaSites84@data, panel == "PanelOne"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(harshaSites84@data, panel == "PanelOne"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
  coord_equal() +
  ggtitle("Main Measurement locations for Harsha Lake -- Dry Run 2")    #printed version made with set.seed value of 444865

ggsave(filename=paste(rootDir, "harshaDemo2/harshaMainSites2.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(harshaSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=harshaEqArea84.f, aes(long, lat, group=group, fill=Strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(harshaSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(harshaSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +
  coord_equal() +
  ggtitle("Oversample locations for Harsha Lake -- Dry Run 2")

ggsave(filename=paste(rootDir, "harshaDemo2/harshaOversampleSites2.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Third map contains all sites
ggmap(harshaSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=harshaEqArea84.f, aes(long, lat, group=group, fill=Strata)) +
  scale_fill_manual(values = c("#000066", "#666699")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=harshaSites84@data, 
             aes(x=long, y=lat, color = panel),
             size = 2) +
  geom_text(data=harshaSites84@data,
            aes(label=siteID, x=long, y=lat, color=panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend
  scale_color_discrete(name = "Sites",
                       labels = c("Oversample", "Main")) +
  coord_equal() +
  ggtitle("All Measurement locations for Harsha Lake -- Dry Run 2")

ggsave(filename=paste(rootDir, "harshaDemo2/harshaAllSites2.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")







