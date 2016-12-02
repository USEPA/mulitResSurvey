## SCRIPT FOR READING CHLOROPHYLL

# LIBRARY----------
# source("ohio2016/scriptsAndRmd/masterLibrary.R")

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