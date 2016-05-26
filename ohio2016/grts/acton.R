# ACTON LAKE GRTS DESIGN
# SAMPLE LAKE FOR WEEK 1 OF STUDY, MAY 30 - JUNE 3
# SMALL LAKE (2.4 km^2) WITH NO OBVIOUS 'TRIBUTARY AREA'.
# WILL USE AN UNSTRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ/PLOT SHAPEFILE ---------
actonEqArea <- readOGR(dsn = paste(rootDir, "acton", sep=""), # Could use read.shape from spsurvey package
                           layer = "actonEqArea")  # shapefile name
plot(actonEqArea) # visualize polygon



# EXTRACT ATTRIBUTE TABLE -----------
# This is referenced in the GRTS att.frame argument.  If src.frame is
# 'shapefile' and the shapefile was created with the sp2shape function, then the the shapefile 
# is stored on disk and does not exist in environment.  Therefore we need to create an obect in
# the environment that contains the information needed for the att.frame argument of the grts
# function.  IF we could get grts to work while specifying src.frame = 'sp.object', then the 
# spatial object (i.e. shapefile) could be read into environment and we could reference 
# shapefile@data in the att.frame argument.  This worked with the UT_ecoregions demo, but we
# can't get it to work here.  
attActon <- read.dbf(filename = paste(rootDir, "acton/actonEqArea", sep=""))



# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
actonDsgn <- list(None=list(panel=c(mainSites=15), # unstratified, therefore 1 list
                            seltype="Equal",
                            over = 20)) # Equal probability, have been using 20 for oversample sites


# Execute survey design  
actonSites <- grts(design=actonDsgn,
                   DesignID="U", # U for unstratified
                   type.frame="area",
                   src.frame="shapefile",
                   in.shape=paste(rootDir, "acton/actonEqArea", sep=""),
                   att.frame=attActon,
                   shapefile=TRUE,
                   prjfilename = paste(rootDir, "acton/actonEqArea", sep=""),
                   out.shape=paste(rootDir, "acton/actonSites", sep=""))

# Print the initial six lines of the survey design ------------
head(actonSites@data)


# Print the survey design summary
summary(actonSites)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
actonEqArea84 <- spTransform(x = actonEqArea, 
                             CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = actonEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "acton", sep=""), 
         layer = "actonEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
actonEqArea84@data$id = rownames(actonEqArea84@data)
actonEqArea84.f <- fortify(actonEqArea84, region="id")  # fortify polygon for ggmap/ggplot
actonEqArea84.f <- merge(actonEqArea84.f, actonEqArea84@data, 
                         by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
actonSitesPlot <- readOGR(dsn = paste(rootDir, "acton", sep=""), 
                          layer = "actonSites")  # shapefile created with grts function
actonSites84 <- spTransform(x = actonSitesPlot, #reproject
                            CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
actonSites84@data <- mutate(actonSites84@data, 
                            long=coordinates(actonSites84)[,1], # add long to @data slot
                            lat=coordinates(actonSites84)[,2], # add lat to @data slot
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
write.table(filter(actonSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "acton/actonOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")
            

# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = actonSites84, 
         dsn = paste(rootDir, "acton", sep=""), 
         layer = "actonSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
actonSites84ListByPanel <- split(actonSites84, # split preserves class, outputs a list
                                 f= actonSites84@data$panel)
# to look at the list (optional):
actonSites84ListByPanel[[1]]@data        #should be the main sites
actonSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = actonSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "acton", sep=""), 
         layer = "actonSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = actonSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "acton", sep=""), 
         layer = "actonSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(actonEqArea)
points(actonSites$xcoord, actonSites$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=actonSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.4) # f is zoom.  Large #, less zoom. tweak for each lake.  
actonSat <- get_map(location = bbox,
                    color = "color",
                    source = "google",
                    maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(actonSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=actonEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(actonSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(actonSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for Acton Lake")

ggsave(filename=paste(rootDir, "acton/actonmainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(actonSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=actonEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=actonEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(actonSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(actonSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for Acton Lake")

ggsave(filename=paste(rootDir, "acton/actonOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# Third map contains all sites
ggmap(actonSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=actonEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=actonSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=actonSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Acton Lake")

ggsave(filename=paste(rootDir, "acton/actonAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



