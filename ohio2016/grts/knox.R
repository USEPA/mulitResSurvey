# STRATIFIED-EQUAL PROBABILITY GRTS DESIGN GOLD STANDARD
    ## ORIGINALLY WRITTEN FOR ROCKY FORK LAKE

# MODIFIED TO APPLY TO KNOX LAKE:
# OPEN WATER MAIN SITES: 10, OVER 20
# TRIB MAIN SITES: 5, OVER 15

# KNOX LAKE DESIGN
# SMALL LAKE WITH TRIBUTARY ON THE NE SIDE
# SHALLOW NE BASIN (<5'); TALKED WITH FISHING MANAGER (6/7/16) WHO REPORTED THAT
# IT IS DIFFICULT TO NAVIGATE ANYWHERE UPSTREAM OF THE TWO BOAT LAUNCHES ON OLD MANSFIELD ROAD
# HE HIMSELF DOESN'T RISK IT, DUE TO STUMPS, SHALLOW WATER
#### knoxEqArea SHAPE FILE WAS MODIFIED TO EXCLUDE APPROX. HALF OF THE NE BASIN 6/7/16 BY SW
# LAKE IS DIVIDED INTO TWO STRATA: 'TRIB' AND 'OPEN-WATER'.
# WILL USE A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"


# READ/PLOT SHAPEFILE ---------
knoxEqArea <- readOGR(dsn = paste(rootDir, "knox", sep=""), # Could use read.shape from spsurvey package
                       layer = "knoxEqArea")  # shapefile name
plot(knoxEqArea) # visualize polygon



# EXTRACT ATTRIBUTE TABLE -----------

attknox <- read.dbf(filename = paste(rootDir, "knox/knoxEqArea", sep=""))



# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
knoxDsgn <- list("open_water" = list(panel=c(mainSites=10),
                                      seltype="Equal",
                                      over=20),
                  "trib"=list(panel=c(mainSites=5),
                              seltype="Equal",
                              over=15))


# Execute survey design  
knoxSitesEqArea <- grts(design=knoxDsgn,
                         DesignID="S", # s for stratified
                         type.frame="area",
                         src.frame="shapefile",
                         in.shape=paste(rootDir, "knox/knoxEqArea", sep=""),
                         att.frame=attknox,
                         stratum="strata",
                         shapefile=TRUE,
                         prjfilename = paste(rootDir, "knox/knoxEqArea", sep=""),
                         out.shape=paste(rootDir, "knox/knoxSitesEqArea", sep=""))


# Print the initial six lines of the survey design ------------
head(knoxSitesEqArea@data)
# Print the survey design summary
summary(knoxSitesEqArea)

knoxSitesEqArea <- readOGR(dsn = paste(rootDir, "knox", sep=""), # Could use read.shape from spsurvey package
                            layer = "knoxSitesEqArea")  # shapefile name

knoxSitesEqArea@data <- mutate(knoxSitesEqArea@data, 
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
head(knoxSitesEqArea@data)


# Print the survey design summary
summary(knoxSitesEqArea)


#new feature added 14 June 2016
writeOGR(obj = knoxSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "knox", sep=""), 
         layer = "knoxSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
knoxEqArea84 <- spTransform(x = knoxEqArea, 
                             CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = knoxEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "knox", sep=""), 
         layer = "knoxEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
knoxEqArea84@data$id = rownames(knoxEqArea84@data)
knoxEqArea84.f <- fortify(knoxEqArea84, region="id")  # fortify polygon for ggmap/ggplot
knoxEqArea84.f <- merge(knoxEqArea84.f, knoxEqArea84@data, 
                         by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
knoxSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "knox", sep=""), 
                                layer = "knoxSitesEqArea")  # shapefile created with grts function
knoxSites84 <- spTransform(x = knoxSitesEqAreaPlot, #reproject
                            CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
knoxSites84@data <- mutate(knoxSites84@data, 
                            long=coordinates(knoxSites84)[,1], # add long to @data slot
                            lat=coordinates(knoxSites84)[,2] # add lat to @data slot
                            
)

# write out table of overdraw sites for reference in field
write.table(filter(knoxSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "knox/knoxOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")


# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = knoxSites84, 
         dsn = paste(rootDir, "knox", sep=""), 
         layer = "knoxSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
knoxSites84ListByPanel <- split(knoxSites84, # split preserves class, outputs a list
                                 f= knoxSites84@data$panel)
# to look at the list (optional):
knoxSites84ListByPanel[[1]]@data        #should be the main sites
knoxSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = knoxSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "knox", sep=""), 
         layer = "knoxSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = knoxSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "knox", sep=""), 
         layer = "knoxSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(knoxEqArea)
points(knoxSitesEqArea$xcoord, knoxSitesEqArea$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=knoxSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.15) # f is zoom.  Large #, less zoom. tweak for each lake.  
knoxSat <- get_map(location = bbox,
                    color = "color",
                    source = "google",
                    maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(knoxSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=knoxEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(knoxSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(knoxSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for Knox Lake")

ggsave(filename=paste(rootDir, "knox/knoxmainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(knoxSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=knoxEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=knoxEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(knoxSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(knoxSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for Knox Lake")

ggsave(filename=paste(rootDir, "knox/knoxOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# Third map contains all sites
ggmap(knoxSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=knoxEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=knoxSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=knoxSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Knox Lake")

ggsave(filename=paste(rootDir, "knox/knoxAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
