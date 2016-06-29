# STRATIFIED, EQUAL PROBABILITY GRTS DESIGN
## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##    MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES 
##    WANTED IN EACH STRATA, NOMINALLY
##       OPEN WATER MAIN SITES = 10
##       OPEN WATER OVER SAMPLE = 20
##       TRIBUTARY MAIN SITES = 5
##       TRIBUTARY OVER SAMPLE = 10
##    CHANGE THE ZOOM FACTOR ON LINE 179
##    FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME

# PLEASANT HILL RESERVOIR GRTS DESIGN
# 
# MEDIUM-SMALL LAKE (3 km^2) WITH ONE MAIN TRIBUTARY TO THE WEST.
# TRIBUTARY AREA IS SMALL, SO LETS TRY AN:
# STRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ/PLOT SHAPEFILE ---------
pleasantHillEqArea <- readOGR(dsn = paste(rootDir, "pleasantHill", sep=""), # Could use read.shape from spsurvey package
                           layer = "pleasantHillEqArea")  # shapefile name
plot(pleasantHillEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE -----------
attpleasantHill <- read.dbf(filename = paste(rootDir, "pleasantHill/pleasantHillEqArea", sep=""))

# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
pleasantHillDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                          seltype="Equal",
                                          over=20),
                      "trib"=list(panel=c(mainSites=5),
                                  seltype="Equal",
                                  over=10))

# Execute survey design  
pleasantHillSitesEqArea <- grts(design=pleasantHillDsgn,
                             DesignID="S", # s for stratified
                             type.frame="area",
                             src.frame="shapefile",
                             in.shape=paste(rootDir, "pleasantHill/pleasantHillEqArea", sep=""),
                             att.frame=attpleasantHill,
                             stratum="strata",
                             shapefile=TRUE,
                             prjfilename = paste(rootDir, "pleasantHill/pleasantHillEqArea", sep=""),
                             out.shape=paste(rootDir, "pleasantHill/pleasantHillSitesEqArea", sep=""))


# Print the initial six lines of the survey design ------------
head(pleasantHillSitesEqArea@data)


# Print the survey design summary
summary(pleasantHillSitesEqArea)

pleasantHillSitesEqArea <- readOGR(dsn = paste(rootDir, "pleasantHill", sep=""), # Could use read.shape from spsurvey package
                                layer = "pleasantHillSitesEqArea")  # shapefile name

pleasantHillSitesEqArea@data <- mutate(pleasantHillSitesEqArea@data, 
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
head(pleasantHillSitesEqArea@data)

# Print the survey design summary
summary(pleasantHillSitesEqArea)


#new feature added 14 June 2016
writeOGR(obj = pleasantHillSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "pleasantHill", sep=""), 
         layer = "pleasantHillSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
pleasantHillEqArea84 <- spTransform(x = pleasantHillEqArea, 
                                 CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = pleasantHillEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "pleasantHill", sep=""), 
         layer = "pleasantHillEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
pleasantHillEqArea84@data$id = rownames(pleasantHillEqArea84@data)
pleasantHillEqArea84.f <- fortify(pleasantHillEqArea84, region="id")  # fortify polygon for ggmap/ggplot
pleasantHillEqArea84.f <- merge(pleasantHillEqArea84.f, pleasantHillEqArea84@data, 
                             by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
pleasantHillSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "pleasantHill", sep=""), 
                                    layer = "pleasantHillSitesEqArea")  # shapefile created with grts function
pleasantHillSites84 <- spTransform(x = pleasantHillSitesEqAreaPlot, #reproject
                                CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
pleasantHillSites84@data <- mutate(pleasantHillSites84@data, 
                                long=coordinates(pleasantHillSites84)[,1], # add long to @data slot
                                lat=coordinates(pleasantHillSites84)[,2] # add lat to @data slot
                                
)

# write out table of overdraw sites for reference in field
write.table(filter(pleasantHillSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "pleasantHill/pleasantHillOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")


# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = pleasantHillSites84, 
         dsn = paste(rootDir, "pleasantHill", sep=""), 
         layer = "pleasantHillSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
pleasantHillSites84ListByPanel <- split(pleasantHillSites84, # split preserves class, outputs a list
                                     f= pleasantHillSites84@data$panel)
# to look at the list (optional):
pleasantHillSites84ListByPanel[[1]]@data        #should be the main sites
pleasantHillSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = pleasantHillSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "pleasantHill", sep=""), 
         layer = "pleasantHillSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = pleasantHillSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "pleasantHill", sep=""), 
         layer = "pleasantHillSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(pleasantHillEqArea)
points(pleasantHillSitesEqArea$xcoord, pleasantHillSitesEqArea$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=pleasantHillSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.5) # f is zoom.  Large #, less zoom. tweak for each lake.  
pleasantHillSat <- get_map(location = bbox,
                        color = "color",
                        source = "google",
                        maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(pleasantHillSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=pleasantHillEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(pleasantHillSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(pleasantHillSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for Pleasant Hill Reservoir")

ggsave(filename=paste(rootDir, "pleasantHill/pleasantHillmainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(pleasantHillSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=pleasantHillEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=pleasantHillEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(pleasantHillSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(pleasantHillSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for Pleasant Hill Reservoir")

ggsave(filename=paste(rootDir, "pleasantHill/pleasantHillOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# Third map contains all sites
ggmap(pleasantHillSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=pleasantHillEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=pleasantHillSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=pleasantHillSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Pleasant Hill Reservoir")

ggsave(filename=paste(rootDir, "pleasantHill/pleasantHillAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
