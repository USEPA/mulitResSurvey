# THIS CODE WILL BE USED TO READ THE POPULATED SHAPEFILES FROM THE
# L DRIVE.  MICHELLE PLATZ ENTERED DATA INTO THE SHAPEFILE.  SINCE
# WE REALLY DON'T NEED THE FULL SHAPEFILE FOR THE ANALYSIS, WE WILL
# JUST WORK WITH THE .dbf FILE.

# LIBRARY----------
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# READ IN .dbf FILES-------------------

# 1. Create a list of files to read in.  The completed data files all
# contain the pattern ...SitesEqAreaData.dbf and are stored in 
# L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/
# directory.

rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/grtsDesign/"

fileNames <- list.files(path = rootDir, 
                        pattern = "SitesEqAreaData.dbf", # file names containing this pattern
                        recursive = TRUE) # look in all subdirectories
# 2.  Read in files
mylist <- list()  # Create an empty list to hold data

for (i in 1:length(fileNames)){  # for each file
  # read.dbf is in foreign and spsurvey.  The foreign version allows for
  # use of as.is argument.
  data.i <- foreign::read.dbf(paste(rootDir, fileNames[i], sep = ""),
                    as.is=TRUE)
  mylist[[i]] <- data.i
}

eqAreaData <- do.call("rbind", mylist)  # This coerces the list into a dataframe. Cool...

pooList <- list()
for (i in 1:length(fileNames)) {
  pooList[[i]] <- 
  colnames(mylist[[12]]) == colnames(mylist[[11]])
}

pooList

lapply(mylist, ncol)

colnames(mylist[[14]]) #alum, harsha, tappan, 54 cols
colnames(mylist[[11]]) #atwood, caesar, harsha, piedmont, 54 cols

colnames(mylist[[6]]) %in% colnames(mylist[[14]])
