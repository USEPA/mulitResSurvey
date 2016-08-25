# STRATIFIED-EQUAL PROBABILITY GRTS DESIGN GOLD STANDARD, ORIGINALLY WRITTEN FOR ROCKY FORK LAKE

# MODIFIED TO APPLY TO MOHAWK LAKE:
# OPEN WATER MAIN SITES: 7, OVER 20
# TRIB MAIN SITES: 8, OVER 15


# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"


# READ/PLOT SHAPEFILE ---------
mohawkEqArea <- readOGR(dsn = paste(rootDir, "mohawk", sep=""), # Could use read.shape from spsurvey package
                       layer = "mohawkEqArea")  # shapefile name
plot(mohawkEqArea) # visualize polygon



# EXTRACT ATTRIBUTE TABLE -----------
attmohawk <- read.dbf(filename = paste(rootDir, "mohawk/mohawkEqArea", sep=""))

# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
mohawkDsgn <- list("open_water" = list(panel=c(mainSites=7),
                                      seltype="Equal",
                                      over=20),
                  "trib"=list(panel=c(mainSites=8),
                              seltype="Equal",
                              over=15))

# Execute survey design  
mohawkSitesEqArea <- grts(design=mohawkDsgn,
                         DesignID="S", # s for stratified
                         type.frame="area",
                         src.frame="shapefile",
                         in.shape=paste(rootDir, "mohawk/mohawkEqArea", sep=""),
                         att.frame=attmohawk,
                         stratum="strata",
                         shapefile=TRUE,
                         prjfilename = paste(rootDir, "mohawk/mohawkEqArea", sep=""),
                         out.shape=paste(rootDir, "mohawk/mohawkSitesEqArea", sep=""))


# Print the initial six lines of the survey design ------------
head(mohawkSitesEqArea@data)

# Print the survey design summary
summary(mohawkSitesEqArea)

mohawkSitesEqArea <- readOGR(dsn = paste(rootDir, "mohawk", sep=""), # Could use read.shape from spsurvey package
                            layer = "mohawkSitesEqArea")  # shapefile name

mohawkSitesEqArea@data <- mutate(mohawkSitesEqArea@data, 
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


#new feature added 14 June 2016
writeOGR(obj = mohawkSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "mohawk", sep=""), 
         layer = "mohawkSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
mohawkEqArea84 <- spTransform(x = mohawkEqArea, 
                             CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = mohawkEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "mohawk", sep=""), 
         layer = "mohawkEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
mohawkEqArea84@data$id = rownames(mohawkEqArea84@data)
mohawkEqArea84.f <- fortify(mohawkEqArea84, region="id")  # fortify polygon for ggmap/ggplot
mohawkEqArea84.f <- merge(mohawkEqArea84.f, mohawkEqArea84@data, 
                         by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
mohawkSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "mohawk", sep=""), 
                                layer = "mohawkSitesEqArea")  # shapefile created with grts function
mohawkSites84 <- spTransform(x = mohawkSitesEqAreaPlot, #reproject
                            CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
mohawkSites84@data <- mutate(mohawkSites84@data, 
                            long=coordinates(mohawkSites84)[,1], # add long to @data slot
                            lat=coordinates(mohawkSites84)[,2] # add lat to @data slot
                            
)

# write out table of overdraw sites for reference in field
write.table(filter(mohawkSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "mohawk/mohawkOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")


# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = mohawkSites84, 
         dsn = paste(rootDir, "mohawk", sep=""), 
         layer = "mohawkSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
mohawkSites84ListByPanel <- split(mohawkSites84, # split preserves class, outputs a list
                                 f= mohawkSites84@data$panel)
# to look at the list (optional):
mohawkSites84ListByPanel[[1]]@data        #should be the main sites
mohawkSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = mohawkSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "mohawk", sep=""), 
         layer = "mohawkSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = mohawkSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "mohawk", sep=""), 
         layer = "mohawkSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(mohawkEqArea)
points(mohawkSitesEqArea$xcoord, mohawkSitesEqArea$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=mohawkSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 1) # f is zoom.  Large #, less zoom. tweak for each lake.  
mohawkSat <- get_map(location = bbox,
                    color = "color",
                    source = "google",
                    maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(mohawkSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=mohawkEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(mohawkSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(mohawkSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for mohawk Lake")

ggsave(filename=paste(rootDir, "mohawk/mohawkmainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(mohawkSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=mohawkEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=mohawkEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(mohawkSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(mohawkSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for mohawk Lake")

ggsave(filename=paste(rootDir, "mohawk/mohawkOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# Third map contains all sites
ggmap(mohawkSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=mohawkEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=mohawkSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=mohawkSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for mohawk Lake")

ggsave(filename=paste(rootDir, "mohawk/mohawkAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")