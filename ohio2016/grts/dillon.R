# STRATIFIED-EQUAL PROBABILITY GRTS DESIGN GOLD STANDARD
# ORIGINALLY WRITTEN FOR ROCKY FORK LAKE

# MODIFIED TO APPLY TO DILLON LAKE:
# OPEN WATER MAIN SITES: 8, OVER 10
# TRIB MAIN SITES: 7, OVER 10

# DILLON RESERVOIR DESIGN
# MEDIUM LAKE (4.16 KM2) WITH LARGE TRIBUTARIES ON THE NW AND NE SIDES

#### dillonEqArea SHAPE FILE WAS MODIFIED TO EXCLUDE THE FURTHEST UPSTREAM PARTS OF THE TRIBS ON 6/17/16 BY JB
# LAKE IS DIVIDED INTO TWO STRATA: 'TRIB' AND 'OPEN-WATER'.
# WILL USE A STRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"


# READ/PLOT SHAPEFILE ---------
dillonEqArea <- readOGR(dsn = paste(rootDir, "dillon", sep=""), # Could use read.shape from spsurvey package
                          layer = "dillonEqArea")  # shapefile name
plot(dillonEqArea) # visualize polygon



# EXTRACT ATTRIBUTE TABLE -----------
attdillon <- read.dbf(filename = paste(rootDir, "dillon/dillonEqArea", sep=""))

# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
dillonDsgn <- list("open_water" = list(panel=c(mainSites=8),
                                         seltype="Equal",
                                         over=10),
                     "trib"=list(panel=c(mainSites=7),
                                 seltype="Equal",
                                 over=10))


# Execute survey design  
dillonSitesEqArea <- grts(design=dillonDsgn,
                            DesignID="S", # s for stratified
                            type.frame="area",
                            src.frame="shapefile",
                            in.shape=paste(rootDir, "dillon/dillonEqArea", sep=""),
                            att.frame=attdillon,
                            stratum="strata",
                            shapefile=TRUE,
                            prjfilename = paste(rootDir, "dillon/dillonEqArea", sep=""),
                            out.shape=paste(rootDir, "dillon/dillonSitesEqArea", sep=""))


# Print the initial six lines of the survey design ------------
head(dillonSitesEqArea@data)


# Print the survey design summary
summary(dillonSitesEqArea)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(dillonEqArea)
points(dillonSitesEqArea$xcoord, dillonSitesEqArea$ycoord)

# Read in points shapefile, append with new attribute table fields, then write to disk
dillonSitesEqArea <- readOGR(dsn = paste(rootDir, "dillon", sep=""), # Could use read.shape from spsurvey package
                               layer = "dillonSitesEqArea")  # shapefile name

dillonSitesEqArea@data <- mutate(dillonSitesEqArea@data, 
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
writeOGR(obj = dillonSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "dillon", sep=""), 
         layer = "dillonSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
dillonEqArea84 <- spTransform(x = dillonEqArea, 
                                CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = dillonEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "dillon", sep=""), 
         layer = "dillonEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
dillonEqArea84@data$id = rownames(dillonEqArea84@data)
dillonEqArea84.f <- fortify(dillonEqArea84, region="id")  # fortify polygon for ggmap/ggplot
dillonEqArea84.f <- merge(dillonEqArea84.f, dillonEqArea84@data, 
                            by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
dillonSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "dillon", sep=""), 
                                   layer = "dillonSitesEqArea")  # shapefile created with grts function
dillonSites84 <- spTransform(x = dillonSitesEqAreaPlot, #reproject
                               CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
dillonSites84@data <- mutate(dillonSites84@data, 
                               long=coordinates(dillonSites84)[,1], # add long to @data slot
                               lat=coordinates(dillonSites84)[,2] # add lat to @data slot
                               
)

# write out table of overdraw sites for reference in field
write.table(filter(dillonSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "dillon/dillonOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")


# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = dillonSites84, 
         dsn = paste(rootDir, "dillon", sep=""), 
         layer = "dillonSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
dillonSites84ListByPanel <- split(dillonSites84, # split preserves class, outputs a list
                                    f= dillonSites84@data$panel)
# to look at the list (optional):
dillonSites84ListByPanel[[1]]@data        #should be the main sites
dillonSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = dillonSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "dillon", sep=""), 
         layer = "dillonSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = dillonSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "dillon", sep=""), 
         layer = "dillonSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=dillonSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.3) # f is zoom.  Large #, less zoom. tweak for each lake.  
dillonSat <- get_map(location = bbox,
                       color = "color",
                       source = "google",
                       maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(dillonSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=dillonEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(dillonSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(dillonSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for Dillon Lake")

ggsave(filename=paste(rootDir, "dillon/dillonmainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(dillonSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=dillonEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=dillonEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(dillonSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(dillonSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for Dillon Lake")

ggsave(filename=paste(rootDir, "dillon/dillonOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# Third map contains all sites
ggmap(dillonSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=dillonEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  scale_fill_manual(values = c("#000066", "#333399")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=dillonSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=dillonSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Dillon Lake")

ggsave(filename=paste(rootDir, "dillon/dillonAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
