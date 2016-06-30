# SENECAVILLE LAKE DESIGN

# STRATIFIED EQUAL
# LARGE LAKE (14.07 km2)
# TWO LARGE TRIBUTARIES ENTER ON EAST SIDE OF LAKE.  TRIMMED OFF AREAS ISOLATED
# EAST AND SOUTH OF 147.  ALSO TRIMMED OFF AREAS FILLED WITH LILLY PADS.
# LARGE LAKE, BUT UNRESTRICTED WAKE IN MOST AREAS, THEREFORE USING EQUAL PROBABILITY.

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ/PLOT SHAPEFILE ---------
senecavilleEqArea <- readOGR(dsn = paste(rootDir, "senecaville", sep=""), # Could use read.shape from spsurvey package
                            layer = "senecavilleEqArea")  # shapefile name
plot(senecavilleEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE -----------
attsenecaville <- read.dbf(filename = paste(rootDir, "senecaville/senecavilleEqArea", sep=""))

# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
senecavilleDsgn <- list("open_water" = list(panel=c(mainSites=8),
                                           seltype="Equal",
                                           over=11),
                       "trib"=list(panel=c(mainSites=7),
                                   seltype="Equal",
                                   over=15))


# Execute survey design  
senecavilleSitesEqArea <- grts(design=senecavilleDsgn,
                              DesignID="S", # s for stratified
                              type.frame="area",
                              src.frame="shapefile",
                              in.shape=paste(rootDir, "senecaville/senecavilleEqArea", sep=""),
                              att.frame=attsenecaville,
                              stratum="strata",
                              shapefile=TRUE,
                              prjfilename = paste(rootDir, "senecaville/senecavilleEqArea", sep=""),
                              out.shape=paste(rootDir, "senecaville/senecavilleSitesEqArea", sep=""))

# Review the survey design
select(senecavilleSitesEqArea@data, mdcaty, stratum, panel) %>%
  arrange(stratum, mdcaty,panel)

# Print the survey design summary
summary(senecavilleSitesEqArea)

plot(senecavilleEqArea)
points(senecavilleSitesEqArea$xcoord, senecavilleSitesEqArea$ycoord)

# new feature added 14 June 2016
# Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
senecavilleSitesEqArea <- readOGR(dsn = paste(rootDir, "senecaville", sep=""), # Could use read.shape from spsurvey package
                                 layer = "senecavilleSitesEqArea")  # shapefile name

senecavilleSitesEqArea@data <- mutate(senecavilleSitesEqArea@data, 
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
head(senecavilleSitesEqArea@data)


#new feature added 14 June 2016
writeOGR(obj = senecavilleSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "senecaville", sep=""), 
         layer = "senecavilleSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
senecavilleEqArea84 <- spTransform(x = senecavilleEqArea, 
                                  CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = senecavilleEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "senecaville", sep=""), 
         layer = "senecavilleEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
senecavilleEqArea84@data$id = rownames(senecavilleEqArea84@data)
senecavilleEqArea84.f <- fortify(senecavilleEqArea84, region="id")  # fortify polygon for ggmap/ggplot
senecavilleEqArea84.f <- merge(senecavilleEqArea84.f, senecavilleEqArea84@data, 
                              by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
senecavilleSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "senecaville", sep=""), 
                                     layer = "senecavilleSitesEqArea")  # shapefile created with grts function
senecavilleSites84 <- spTransform(x = senecavilleSitesEqAreaPlot, #reproject
                                 CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
senecavilleSites84@data <- mutate(senecavilleSites84@data, 
                                 long=coordinates(senecavilleSites84)[,1], # add long to @data slot
                                 lat=coordinates(senecavilleSites84)[,2] # add lat to @data slot
                                 
)

# write out table of overdraw sites for reference in field
write.table(filter(senecavilleSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "senecaville/senecavilleOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")


# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = senecavilleSites84, 
         dsn = paste(rootDir, "senecaville", sep=""), 
         layer = "senecavilleSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
senecavilleSites84ListByPanel <- split(senecavilleSites84, # split preserves class, outputs a list
                                      f= senecavilleSites84@data$panel)
# to look at the list (optional):
senecavilleSites84ListByPanel[[1]]@data        #should be the main sites
senecavilleSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = senecavilleSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "senecaville", sep=""), 
         layer = "senecavilleSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = senecavilleSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "senecaville", sep=""), 
         layer = "senecavilleSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(senecavilleEqArea)
points(senecavilleSitesEqArea$xcoord, senecavilleSitesEqArea$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=senecavilleSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.5) # f is zoom.  Large #, less zoom. tweak for each lake.  
senecavilleSat <- get_map(location = bbox,
                         color = "color",
                         source = "google",
                         maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(senecavilleSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=senecavilleEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(senecavilleSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(senecavilleSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for Senecaville Lake")

ggsave(filename=paste(rootDir, "senecaville/senecavillemainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(senecavilleSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=senecavilleEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=senecavilleEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(senecavilleSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(senecavilleSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for Senecaville Lake")

ggsave(filename=paste(rootDir, "senecaville/senecavilleOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")


# Third map contains all sites
ggmap(senecavilleSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=senecavilleEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=senecavilleSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=senecavilleSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Senecaville Lake")

ggsave(filename=paste(rootDir, "senecaville/senecavilleAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")