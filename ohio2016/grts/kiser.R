
# UNSTRATIFIED, EQUAL PROBABILITY GRTS DESIGN
## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
## MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED
### NOMINAL MAINSITES = 15
### NOMINAL OVER SAMPLE = 20
## CHANGE THE ZOOM FACTOR ON LINE 179 
## FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME

# DEMO:  KISER LAKE DESIGN (UPDATED 6/17/16)
# SHALLOW LAKE WITH NO OBVIOUS 'TRIBUTARY AREA'.
# WILL USE AN UNSTRATIFIED-EQUAL PROBABILITY GRTS DESIGN


# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY ---------
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READ/PLOT SHAPEFILE ---------
kiserEqArea <- readOGR(dsn = paste(rootDir, "kiserLakeDemo", sep=""), # Could use read.shape from spsurvey package
                       layer = "kiserLakeEqArea")  # shapefile name
plot(kiserLakeEqArea) # visualize polygon



# EXTRACT ATTRIBUTE TABLE -----------  
attkiser <- read.dbf(filename = paste(rootDir, "kiserLakeDemo/kiserLakeEqArea", sep=""))

# SET UP FOR AND RUN GRTS FUNCTION   -------------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)      #4447864

# Create the design list
kiserDsgn <- list(None=list(panel=c(mainSites=15), # unstratified, therefore 1 list
                            seltype="Equal",
                            over = 20)) # Equal probability, have been using 20 for oversample sites


# Execute survey design  
kiserSitesEqArea <- grts(design=kiserDsgn,
                         DesignID="U", # U for unstratified
                         type.frame="area",
                         src.frame="shapefile",
                         in.shape=paste(rootDir, "kiserLakeDemo/kiserLakeEqArea", sep=""),
                         att.frame=attkiser,
                         shapefile=TRUE,
                         prjfilename = paste(rootDir, "kiserLakeDemo/kiserLakeEqArea", sep=""),
                         out.shape=paste(rootDir, "kiserLakeDemo/kiserSitesEqArea", sep=""))

# new feature added 14 June 2016
# Set up for mutating the kiserSitesEqArea file to include all of the fill-able fields
# necessary in order to keep the Equal Area projection
kiserSitesEqArea <- readOGR(dsn = paste(rootDir, "kiserLakeDemo", sep=""), # Could use read.shape from spsurvey package
                            layer = "kiserSitesEqArea")  # shapefile name

# add fill-able fields, preparation for analyzing GRTS results
kiserSitesEqArea@data <- mutate(kiserSitesEqArea@data, 
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
writeOGR(obj = kiserSitesEqArea,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
         layer = "kiserSitesEqArea",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# Print the initial six lines of the survey design ------------
head(kiserSitesEqArea@data)


# Print the survey design summary
summary(kiserSitesEqArea)

# PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
kiserEqArea84 <- spTransform(x = kiserEqArea, 
                             CRS("+proj=longlat +datum=WGS84")) # specifies projection
writeOGR(obj = kiserEqArea84,  # write projected shapefile to disk for use on field computer
         dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
         layer = "kiserEqArea84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
kiserEqArea84@data$id = rownames(kiserEqArea84@data)
kiserEqArea84.f <- fortify(kiserEqArea84, region="id")  # fortify polygon for ggmap/ggplot
kiserEqArea84.f <- merge(kiserEqArea84.f, kiserEqArea84@data, 
                         by="id")  # bring attributes back in 

# Read and project spatial points dataframe for plotting
kiserSitesPlot <- readOGR(dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
                          layer = "kiserSitesEqArea")  # shapefile created with grts function
kiserSites84 <- spTransform(x = kiserSitesPlot, #reproject
                            CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
kiserSites84@data <- mutate(kiserSites84@data, 
                            long=coordinates(kiserSites84)[,1], # add long to @data slot
                            lat=coordinates(kiserSites84)[,2]) # add lat to @data slot

# write out table of overdraw sites for reference in field
write.table(filter(kiserSites84@data, panel == "OverSamp")   %>%
              select(siteID, stratum, long, lat),
            file = paste(rootDir, "kiserLakeDemo/kiserOverSampList.txt", sep=""),
            row.names = FALSE, sep="\t")


# write projected shapefile to disk for use on field computer.
# arcPad requires one shapefile with all sites.
# write one shapefile with all sites for use with arcPad
writeOGR(obj = kiserSites84, 
         dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
         layer = "kiserSites84",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# iPad and geoplatform require separate shapefiles for main and oversamp sites.
# separate shapefile for mainSites (main sites) and OverSamp (overdraw sites)
kiserSites84ListByPanel <- split(kiserSites84, # split preserves class, outputs a list
                                 f= kiserSites84@data$panel)
# to look at the list (optional):
kiserSites84ListByPanel[[1]]@data        #should be the main sites
kiserSites84ListByPanel[[2]]@data        #should be OverSamp sites

# Write 'mainSites' shapefile to disk
writeOGR(obj = kiserSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
         layer = "kiserSites84mainSites",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
# Write 'OverSamp' shapefile to disk
writeOGR(obj = kiserSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
         dsn = paste(rootDir, "kiserLakeDemo", sep=""), 
         layer = "kiserSites84OverSamp",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# SIMPLE VISUALIZATION WITH BASIC R --------
plot(kiserEqArea)
points(kiserSitesEqArea$xcoord, kiserSitesEqArea$ycoord)

# VISUALIZE SURVEY DESIGN WITH GGMAPS--------
# Get ggmap
bbox <- make_bbox(data=kiserSites84@data, #defines map extent based on sample site lat/lon
                  long, lat, f = 0.25) # f is zoom.  Large #, less zoom. tweak for each lake.  
kiserSat <- get_map(location = bbox,
                    color = "color",
                    source = "google",
                    maptype = "satellite")

# Plot ggmap with sites
# First map contains only main sample sites
ggmap(kiserSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=kiserEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(kiserSites84@data, panel == "mainSites"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#F8766D") + # specify color to be consistent across maps
  geom_text(data=filter(kiserSites84@data, panel == "mainSites"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#F8766D") +    #alternate color: "#00BFC4"
  coord_equal() +
  ggtitle("Main Measurement locations for Kiser Lake")

ggsave(filename=paste(rootDir, "kiserLakeDemo/kisermainSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# Second map contains only oversample sites
ggmap(kiserSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=kiserEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  #geom_polygon(data=kiserEqArea84.f, aes(long, lat, group=group, fill=strata)) +
  #scale_fill_manual(values = c("#000066", "#333366")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=filter(kiserSites84@data, panel == "OverSamp"), #only main sites
             aes(x=long, y=lat),
             size = 2, color = "#00BFC4") + # specify color to be consistent across maps
  geom_text(data=filter(kiserSites84@data, panel == "OverSamp"), #only main sites
            aes(label=siteID, x=long, y=lat),
            hjust=1.2, vjust=0, size=2, color = "#00BFC4") +      #alternate color: "#F8766D"
  coord_equal() +
  ggtitle("Oversample locations for Kiser Lake")

ggsave(filename=paste(rootDir, "kiserLakeDemo/kiserOversampleSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# Third map contains all sites
ggmap(kiserSat) +
  ylab("Latitude") +
  xlab ("Longitude") +
  geom_polygon(data=kiserEqArea84.f, aes(long, lat, group=group, fill="Open Water")) +
  scale_fill_manual(values = c("#000066")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  geom_point(data=kiserSites84@data, 
             aes(x=long, y=lat, color = panel),       
             size = 2) +
  geom_text(data=kiserSites84@data,
            aes(label=siteID, x=long, y=lat, color= panel),
            hjust=1.2, vjust=0, # horizontal and vertical adjustment
            size=2, show.legend = FALSE) + # text size and call to suppres legend 
  scale_color_discrete(name = "Sites",
                       labels = c("Main", "Oversample")) +
  coord_equal() +
  ggtitle("All Measurement locations for Kiser Lake")

ggsave(filename=paste(rootDir, "kiserLakeDemo/kiserAllSites.tiff", sep=""),
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")


