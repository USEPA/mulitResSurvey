# CAVE RUN LAKE DESIGN

# CURRENT DESIGN IS FOR UNSTRATIFIED EQUAL-PROBABILITY
# THIS IS MAINLY FOR THE PURPOSES OF QUICKLY GENERATING A MAP FOR
# THE US FOREST SERVICE PERMIT.  DESING WILL CHANGE AFTER WE LEARN MORE INFORMATION
# ABOUT THE RESERVOIR (ie UPSTREAM EXTENT)


# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ/PLOT SHAPEFILE ---------
caveRunEqArea <- readOGR(dsn = paste(rootDir, "caveRun", sep=""), # Could use read.shape from spsurvey package
                       layer = "caveRunEqArea")  # shapefile name
plot(caveRunEqArea) # visualize polygon



# EXTRACT ATTRIBUTE TABLE -----------  
attcaveRun <- read.dbf(filename = paste(rootDir, "caveRun/caveRunEqArea", sep=""))

# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
caveRunDsgn <- list(None=list(panel=c(mainSites=15), # unstratified, therefore 1 list
                            seltype="Equal",
                            over = 20)) # Equal probability, have been using 20 for oversample sites


# Execute survey design  
caveRunSitesEqArea <- grts(design=caveRunDsgn,
                         DesignID="U", # U for unstratified
                         type.frame="area",
                         src.frame="shapefile",
                         in.shape=paste(rootDir, "caveRun/caveRunEqArea", sep=""),
                         att.frame=attcaveRun,
                         shapefile=TRUE,
                         prjfilename = paste(rootDir, "caveRun/caveRunEqArea", sep=""),
                         out.shape=paste(rootDir, "caveRun/caveRunSitesEqArea", sep=""))

# new feature added 14 June 2016
# Set up for mutating the caveRunSitesEqArea file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
caveRunSitesEqArea <- readOGR(dsn = paste(rootDir, "caveRun", sep=""), # Could use read.shape from spsurvey package
                            layer = "caveRunSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
caveRunSitesEqArea@data <- mutate(caveRunSitesEqArea@data, 
                                deplyDate = "",    # adding all of these colums to the 
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
writeOGR(obj = caveRunSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "caveRun", sep=""), 
         layer = "caveRunSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# Print the initial six lines of the survey design ------------
head(caveRunSitesEqArea@data)


# Print the survey design summary
summary(caveRunSitesEqArea)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
caveRunEqArea84 <- spTransform(x = caveRunEqArea, 
                             CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = caveRunEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "caveRun", sep=""), 
         layer = "caveRunEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
caveRunEqArea84@data$id = rownames(caveRunEqArea84@data)
caveRunEqArea84.f <- fortify(caveRunEqArea84, region="id")  # fortify polygon for ggmap/ggplot
caveRunEqArea84.f <- merge(caveRunEqArea84.f, caveRunEqArea84@data, 
                         by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
caveRunSitesPlot <- readOGR(dsn = paste(rootDir, "caveRun", sep=""), 
                          layer = "caveRunSitesEqArea")  # shapefile created with grts function
caveRunSites84 <- spTransform(x = caveRunSitesPlot, #reproject
                            CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
caveRunSites84@data <- mutate(caveRunSites84@data, 
                            long=coordinates(caveRunSites84)[,1], # add long to @data slot
                            lat=coordinates(caveRunSites84)[,2]) # add lat to @data slot

# write out table of overdraw sites for reference in field
write.table(filter(caveRunSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "caveRun/caveRunOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")


# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = caveRunSites84, 
         dsn = paste(rootDir, "caveRun", sep=""), 
         layer = "caveRunSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
caveRunSites84ListByPanel <- split(caveRunSites84, # split preserves class, outputs a list
                                 f= caveRunSites84@data$panel)
# to look at the list (optional):
caveRunSites84ListByPanel[[1]]@data        #should be the main sites
caveRunSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = caveRunSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "caveRun", sep=""), 
         layer = "caveRunSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = caveRunSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "caveRun", sep=""), 
         layer = "caveRunSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(caveRunEqArea)
points(caveRunSitesEqArea$xcoord, caveRunSitesEqArea$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=caveRunSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.4) # f is zoom.  Large #, less zoom. tweak for each lake.  
caveRunSat <- get_map(location = bbox,
                    color = "color",
                    source = "google",
                    maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(caveRunSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=caveRunEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(caveRunSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
#   geom_text(data=filter(caveRunSites84@data, panel == "mainSites"), #only main sites
#             aes(label=siteID, x=long, y=lat),
#             hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  theme(legend.position="none") +
  ggtitle("Main Measurement locations for caveRun Lake")

ggsave(filename=paste(rootDir, "caveRun/caveRunmainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(caveRunSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=caveRunEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=caveRunEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(caveRunSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(caveRunSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for caveRun Lake")

ggsave(filename=paste(rootDir, "caveRun/caveRunOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# Third map contains all sites
ggmap(caveRunSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=caveRunEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=caveRunSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=caveRunSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for caveRun Lake")

ggsave(filename=paste(rootDir, "caveRun/caveRunAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

