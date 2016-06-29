# STRATIFIED, EQUAL PROBABILITY GRTS DESIGN
## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##    MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES 
##    WANTED IN EACH STRATA, FOR PAINT CREEK:
##       OPEN WATER MAIN SITES = 10
##       OPEN WATER OVER SAMPLE = 20
##       TRIBUTARY MAIN SITES = 5
##       TRIBUTARY OVER SAMPLE = 10
##    CHANGE THE ZOOM FACTOR ON LINE 179
##    FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME

# LAKE LORAMIE LAKE GRTS DESIGN
# SMALL LAKE (3.5 km^2) WITH ONE MAJOR TRIBUTARY ARM
# OPEN WATER = 2.8KM2  TRIB = 0.8KM2
# SHAPEFILE EXTENSIVELY EDITED TO CONFORM WITH AREAL IMAGERY

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ/PLOT SHAPEFILE ---------
loramieEqArea <- readOGR(dsn = paste(rootDir, "loramie", sep=""), # Could use read.shape from spsurvey package
                            layer = "loramieEqArea")  # shapefile name
plot(loramieEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE -----------
attloramie <- read.dbf(filename = paste(rootDir, "loramie/loramieEqArea", sep=""))

# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
loramieDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                           seltype="Equal",
                                           over=20),
                       "trib"=list(panel=c(mainSites=5),
                                   seltype="Equal",
                                   over=10))


# Execute survey design  
loramieSitesEqArea <- grts(design=loramieDsgn,
                              DesignID="S", # s for stratified
                              type.frame="area",
                              src.frame="shapefile",
                              in.shape=paste(rootDir, "loramie/loramieEqArea", sep=""),
                              att.frame=attloramie,
                              stratum="strata",
                              shapefile=TRUE,
                              prjfilename = paste(rootDir, "loramie/loramieEqArea", sep=""),
                              out.shape=paste(rootDir, "loramie/loramieSitesEqArea", sep=""))

# Inspect survey design
head(loramieSitesEqArea@data) # Print first six lines of the survey design
summary(loramieSitesEqArea) # Print the survey design summary
select(loramieSitesEqArea@data, stratum, panel, siteID) # Print oversample sites
plot(brookevilleEqArea) # Plot polygon
points(brookevilleSitesEqArea$xcoord, brookevilleSitesEqArea$ycoord) # plot sites

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
loramieSitesEqArea <- readOGR(dsn = paste(rootDir, "loramie", sep=""), # Could use read.shape from spsurvey package
                                 layer = "loramieSitesEqArea")  # shapefile name

loramieSitesEqArea@data <- mutate(loramieSitesEqArea@data, 
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

# In case you want to check again:
# Print the initial six lines of the survey design ------------
head(loramieSitesEqArea@data)

# Write mutated shapefile to disk
writeOGR(obj = loramieSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "loramie", sep=""), 
         layer = "loramieSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
loramieEqArea84 <- spTransform(x = loramieEqArea, 
                                  CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = loramieEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "loramie", sep=""), 
         layer = "loramieEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
loramieEqArea84@data$id = rownames(loramieEqArea84@data)
loramieEqArea84.f <- fortify(loramieEqArea84, region="id")  # fortify polygon for ggmap/ggplot
loramieEqArea84.f <- merge(loramieEqArea84.f, loramieEqArea84@data, 
                              by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
loramieSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "loramie", sep=""), 
                                     layer = "loramieSitesEqArea")  # shapefile created with grts function
loramieSites84 <- spTransform(x = loramieSitesEqAreaPlot, #reproject
                                 CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
loramieSites84@data <- mutate(loramieSites84@data, 
                                 long=coordinates(loramieSites84)[,1], # add long to @data slot
                                 lat=coordinates(loramieSites84)[,2] # add lat to @data slot
                                 
)

# write out table of overdraw sites for reference in field
write.table(filter(loramieSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "loramie/loramieOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")


# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = loramieSites84, 
         dsn = paste(rootDir, "loramie", sep=""), 
         layer = "loramieSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
loramieSites84ListByPanel <- split(loramieSites84, # split preserves class, outputs a list
                                      f= loramieSites84@data$panel)
# to look at the list (optional):
loramieSites84ListByPanel[[1]]@data        #should be the main sites
loramieSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = loramieSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "loramie", sep=""), 
         layer = "loramieSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = loramieSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "loramie", sep=""), 
         layer = "loramieSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(loramieEqArea)
points(loramieSitesEqArea$xcoord, loramieSitesEqArea$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=loramieSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.5) # f is zoom.  Large #, less zoom. tweak for each lake.  
loramieSat <- get_map(location = bbox,
                         color = "color",
                         source = "google",
                         maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(loramieSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=loramieEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(loramieSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(loramieSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for Lake Loramie")

ggsave(filename=paste(rootDir, "loramie/loramiemainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(loramieSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=loramieEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=loramieEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(loramieSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(loramieSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for Lake Loramie")

ggsave(filename=paste(rootDir, "loramie/loramieOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")


# Third map contains all sites
ggmap(loramieSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=loramieEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=loramieSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=loramieSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Lake Loramie")

ggsave(filename=paste(rootDir, "loramie/loramieAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
