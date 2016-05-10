#  SCRIPT TO DEMO spsurvey PACKAGE FOR THE GRTS DESIGN OF AN AREA RESOURCE

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF L-DRIVE WORKING DIRECTORY
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

# READING IN AND FORMATTING SPATIAL DATA---------
# Load the sp object in the data directory
  data(UT_ecoregions)
  str(UT_ecoregions)
# Create a shapefile
   sp2shape(sp.obj=UT_ecoregions, 
            shpfilename=paste(rootDir, "spsurveyDemo/UT_ecoregionsShp", sep=""))

# Read the attribute table from the shapefile
  att <- read.dbf(paste(rootDir, "spsurveyDemo/UT_ecoregionsShp", sep=""))

# Display the attribute data frame
  att
  str(att)

# Summarize frame area by ecoregion
  temp <- tapply(att$Area_ha, att$Level3_Nam, sum)
  temp <- round(addmargins(temp), 0)
  temp

# Unstratifed, equal probability, GRTS survey design---------
# Call the set.seed function so that the survey designs can be replicate
  set.seed(4447864)

# Create the design list
  Equaldsgn <- list(None=list(panel=c(PanelOne=115), seltype="Equal"))

# Select the sample
Equalsites <- grts(design=Equaldsgn,
                   DesignID="EQUAL", # name that will append site name in output file
                   type.frame="area",
                   src.frame="shapefile",
                   in.shape=paste(rootDir, "spsurveyDemo/UT_ecoregionsShp", sep=""),
                   att.frame=att,
                   shapefile=TRUE,
                   out.shape=paste(rootDir, "spsurveyDemo/Equalsites", sep=""))

# Print the initial six lines of the survey design
   head(Equalsites@data)

 # Print the survey design summary
   summary(Equalsites)

# visualize survey design
plot(UT_ecoregions)
points(Equalsites$xcoord, Equalsites$ycoord)

# Unstratifed, unequal probability, GRTS survey design----------------
Unequaldsgn <- list(None=list(panel=c(PanelOne=115),
                              seltype="Unequal",
                              caty.n=c("Central Basin and Range"=25,
                                       "Colorado Plateaus"=25,
                                       "Mojave Basin and Range"=10,
                                       "Northern Basin and Range"=10,
                                       "Southern Rockies"=10,
                                       "Wasatch and Uinta Mountains"=25,
                                       "Wyoming Basin"=10)))

Unequalsites <- grts(design=Unequaldsgn,
                     DesignID="UNEQUAL", # name that will append site name in output file
                     type.frame="area",
                     src.frame="shapefile",
                     in.shape=paste(rootDir, "spsurveyDemo/UT_ecoregionsShp", sep=""),
                     att.frame=att,
                     mdcaty="Level3_Nam",
                     shapefile=TRUE,  #optional, TRUE if you want shapefile created
                     out.shape=paste(rootDir, "spsurveyDemo/Unequalsites", sep=""))  # shapefile name

 # Print the initial six lines of the survey design
   head(Unequalsites@data)

# Visualize survey design
plot(UT_ecoregions)
points(Unequalsites$xcoord, Unequalsites$ycoord)

# Stratifed, equal probability, GRTS survey design-----------------
# Read the shapefile
 shp <- read.shape(paste(rootDir, "spsurveyDemo/UT_ecoregionsShp", sep="")) # more typical with readOGR function

# Create the design list
Stratdsgn <- list("Central Basin and Range"=list(panel=c(PanelOne=25),                                                 
                                                 seltype="Equal"),
                  "Colorado Plateaus"=list(panel=c(PanelOne=25),
                                           seltype="Equal"),
                  "Mojave Basin and Range"=list(panel=c(PanelOne=10),
                                                seltype="Equal"),
                  "Northern Basin and Range"=list(panel=c(PanelOne=10),
                                                  seltype="Equal"),
                  "Southern Rockies"=list(panel=c(PanelOne=10),
                                          seltype="Equal"),
                  "Wasatch and Uinta Mountains"=list(panel=c(PanelOne=25),
                                                     seltype="Equal"),
                  "Wyoming Basin"=list(panel=c(PanelOne=10),
                                       seltype="Equal"))

Stratsites <- grts(design=Stratdsgn,
                   DesignID="STRATIFIED", # name that will append site name in output file
                   type.frame="area",
                   src.frame="sp.object",
                   sp.object=shp,
                   att.frame=att,
                   stratum="Level3_Nam",
                   shapefile=TRUE,
                   out.shape=paste(rootDir, "spsurveyDemo/EqualStratified", sep=""))

 # Print the initial six lines of the survey design
   head(Stratsites@data)

#Visualize results
plot(shp)
points(Stratsites$xcoord, Stratsites$ycoord)

# Unstratifed, unequal probability, GRTS survey design  with  an  oversample  and  a  panel  structure  for-------
# survey over time

Paneldsgn <- list(None=list(panel=c(Year1=50, Year2=50, Year3=50,
                                    Year4=50, Year5=50),
                            seltype="Unequal",
                            caty.n=c("Central Basin and Range"=64,
                                     "Colorado Plateaus"=63,
                                     "Mojave Basin and Range"=15,
                                     "Northern Basin and Range"=15,
                                     "Southern Rockies"=15,
                                     "Wasatch and Uinta Mountains"=63,
                                     "Wyoming Basin"=15),
                            over=100))

# Select the sample
Panelsites <- grts(design=Paneldsgn,
                   DesignID="UNEQUAL",
                   type.frame="area",
                   src.frame="shapefile",
                   in.shape=paste(rootDir, "spsurveyDemo/UT_ecoregionsShp", sep=""),
                   att.frame=att,
                   mdcaty="Level3_Nam",
                   shapefile=TRUE,
                   out.shape=paste(rootDir, "spsurveyDemo/UnequalOverTime", sep=""))

 # Print the initial six lines of the survey design
   head(Panelsites@data)

 # Print the survey design summary
   summary(Panelsites)

