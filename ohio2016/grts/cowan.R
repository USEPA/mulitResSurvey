# STRATIFIED-EQUAL PROBABILITY GRTS DESIGN GOLD STANDARD, ORIGINALLY WRITTEN FOR ROCKY FORK LAKE

# MODIFIED TO APPLY TO COWAN LAKE:
    # OPEN WATER MAIN SITES: 12, OVER 20
    # TRIB MAIN SITES: 3, OVER 9


# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"


# READ/PLOT SHAPEFILE ---------
cowanEqArea <- readOGR(dsn = paste(rootDir, "cowan", sep=""), # Could use read.shape from spsurvey package
                           layer = "cowanEqArea")  # shapefile name
plot(cowanEqArea) # visualize polygon



# EXTRACT ATTRIBUTE TABLE -----------

attCowan <- read.dbf(filename = paste(rootDir, "cowan/cowanEqArea", sep=""))



# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
cowanDsgn <- list("open_water" = list(panel=c(mainSites=12),
                                          seltype="Equal",
                                          over=20),
                      "trib"=list(panel=c(mainSites=3),
                                  seltype="Equal",
                                  over=9))


# Execute survey design  
cowanSitesEqArea <- grts(design=cowanDsgn,
                             DesignID="S", # s for stratified
                             type.frame="area",
                             src.frame="shapefile",
                             in.shape=paste(rootDir, "cowan/cowanEqArea", sep=""),
                             att.frame=attCowan,
                             stratum="strata",
                             shapefile=TRUE,
                             prjfilename = paste(rootDir, "cowan/cowanEqArea", sep=""),
                             out.shape=paste(rootDir, "cowan/cowanSitesEqArea", sep=""))


# Print the initial six lines of the survey design ------------
head(cowanSitesEqArea@data)


# Print the survey design summary
summary(cowanSitesEqArea)

cowanSitesEqArea <- readOGR(dsn = paste(rootDir, "cowan", sep=""), # Could use read.shape from spsurvey package
                                layer = "cowanSitesEqArea")  # shapefile name

cowanSitesEqArea@data <- mutate(cowanSitesEqArea@data, 
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
head(cowanSitesEqArea@data)


# Print the survey design summary
summary(cowanSitesEqArea)


#new feature added 14 June 2016
writeOGR(obj = cowanSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "cowan", sep=""), 
         layer = "cowanSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
cowanEqArea84 <- spTransform(x = cowanEqArea, 
                                 CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = cowanEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "cowan", sep=""), 
         layer = "cowanEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
cowanEqArea84@data$id = rownames(cowanEqArea84@data)
cowanEqArea84.f <- fortify(cowanEqArea84, region="id")  # fortify polygon for ggmap/ggplot
cowanEqArea84.f <- merge(cowanEqArea84.f, cowanEqArea84@data, 
                             by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
cowanSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "cowan", sep=""), 
                                    layer = "cowanSitesEqArea")  # shapefile created with grts function
cowanSites84 <- spTransform(x = cowanSitesEqAreaPlot, #reproject
                                CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
cowanSites84@data <- mutate(cowanSites84@data, 
                                long=coordinates(cowanSites84)[,1], # add long to @data slot
                                lat=coordinates(cowanSites84)[,2] # add lat to @data slot
                                
)

# write out table of overdraw sites for reference in field
write.table(filter(cowanSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "cowan/cowanOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")


# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = cowanSites84, 
         dsn = paste(rootDir, "cowan", sep=""), 
         layer = "cowanSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
cowanSites84ListByPanel <- split(cowanSites84, # split preserves class, outputs a list
                                     f= cowanSites84@data$panel)
# to look at the list (optional):
cowanSites84ListByPanel[[1]]@data        #should be the main sites
cowanSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = cowanSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "cowan", sep=""), 
         layer = "cowanSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = cowanSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "cowan", sep=""), 
         layer = "cowanSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(cowanEqArea)
points(cowanSitesEqArea$xcoord, cowanSitesEqArea$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=cowanSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 1) # f is zoom.  Large #, less zoom. tweak for each lake.  
cowanSat <- get_map(location = bbox,
                        color = "color",
                        source = "google",
                        maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(cowanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=cowanEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(cowanSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(cowanSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for Cowan Lake")

ggsave(filename=paste(rootDir, "cowan/cowanmainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(cowanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=cowanEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=cowanEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(cowanSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(cowanSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for Cowan Lake")

ggsave(filename=paste(rootDir, "cowan/cowanOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# Third map contains all sites
ggmap(cowanSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=cowanEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=cowanSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=cowanSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Cowan Lake")

ggsave(filename=paste(rootDir, "cowan/cowanAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
