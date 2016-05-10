# DEMO:  KISER LAKE DESIGN


# SHALLOW LAKE WITH NO OBVIOUS 'TRIBUTARY AREA'.
# WILL USE AN UNSTRATIFIED-EQUAL PROBABILITY GRTS DESIGN

# READ/PLOT SHAPEFILE
  kiserLakeEqArea <- readOGR(dsn = "ohio2016/inputData/spatialData", # Could use read.shape from spsurvey package
                      layer = "kiserLakeEqArea")  # shapefile name
    plot(kiserLakeEqArea) # visualize polygon
  
# CREATE SHAPEFILE
# We read in a shapefile, but apparently the sp2shape
# function is needed to prep this object for the grts function.  Also
# tried referencing the object as a 'sp.object' in the grts argument 'src.frame',
# but that failed to (see below).  The approach defined below works, but must specify projection 
# in sp2shape function AND in the grts call.  Seem that we should be able to use
# the readOGR object directly, but can't get it to work.
  
sp2shape(sp.obj=kiserLakeEqArea, # object created with readOGR or read.shape
         prjfilename = "ohio2016/inputData/spatialData/kiserLakeEqArea", # proj from original shapefile
         shpfilename="ohio2016/inputData/spatialData/kiserLakeEqArea_shp") # name of created shapefile.

# EXTRACT ATTRIBUTE TABLE
# This is referenced in the GRTS att.frame argument.  If src.frame is
# 'shapefile' and the shapefile was created with the sp2shape function, then the the shapefile 
# is stored on disk and does not exist in environment.  Therefore we need to create an obect in
# the environment that contains the information needed for the att.frame argument of the grts
# function.  IF we could get grts to work while specifying src.frame = 'sp.object', then the 
# spatial object (i.e. shapefile) could be read into environment and we could reference 
# shapefile@data in the att.frame argument.  This worked with the UT_ecoregions demo, but we
# can't get it to work here.  
attKiser <- read.dbf(filename = "ohio2016/inputData/spatialData/kiserLakeEqArea")

  
# SET UP FOR GRTS FUNCTION  
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)

# Create the design list
kiserDsgn <- list(None=list(panel=c(PanelOne=15), # unstratified, therefore 1 list
                            seltype="Equal",
                            over = 15)) # Equal probability


# Execute survey design  
kiserSites <- grts(design=kiserDsgn,
                   DesignID="EQUAL", # name that will append site name in output file
                   type.frame="area",
                   src.frame="shapefile",
                   in.shape="ohio2016/inputData/spatialData/kiserLakeEqArea_shp",
                   att.frame=attKiser,
                   shapefile=TRUE,
                   prjfilename = "ohio2016/inputData/spatialData/kiserLakeEqArea",
                   out.shape="ohio2016/inputData/spatialData/kiserSites")

# Print the initial six lines of the survey design
head(kiserSites@data)

# Print the survey design summary
summary(kiserSites)

# visualize survey design
plot(kiserLakeEqArea)
points(kiserSites$xcoord, kiserSites$ycoord)
  
  
# # This approach specifies that the source frame is an 'sp.object', then points to the object
# # created using the readOGR function.  As best we can tell, the readOGR object is of class
# # sp, yet the grts function kicks out an error "An sp package object is required when the value provided
# # for argument src.frame equals "sp.object".  We don't know why this approach isn't working.  
#   Select the sample
#   kiserSites <- grts(design=kiserDsgn,
#                      DesignID="EQUAL", # name that will append site name in output file
#                      type.frame="area",
#                      src.frame="sp.object",
#                      in.shape="kiserShp",
#                      att.frame=attKiser,
#                      shapefile=TRUE,
#                      out.shape="kiserSites")


