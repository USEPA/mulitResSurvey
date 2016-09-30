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

colnames(mylist[[5]]) %in% colnames(mylist[[6]])



# 
# STRATEGY TO FACILITATE merge
# Buckhorn, Brookeville, Carr Cr, and Cave Run have 57 columns, compared to 53 or 54 from most other point shapefiles.  Extra columns are due to field included in geodatabase with original polygon shapefile.  
#                              1.	 Rename GNIS_Name in above = Lake_Name in others
#                              2.  omit Don’t need: "OBJECTID"   "Permanent_" "FDate"     
#                              [13] "Resolution" "GNIS_Name"  "Elevation"  "ReachCode"  "FType"     
#                              [19] "FCode"      
#                              
#                              AND
#                              Most 53 column files contain stuff we don’t need: "Connectivi" "Issue_Type"
#                              [13] "Lake_Name_" "Reservoir_" "QC"       
#                              
#                              2.	 Rename deplyDate to deplyDt
#                              3.	Add ‘section’ to equal area designs
#                              4.	Do we need to add ‘stratum’ to Unstratified designs?
#                              5.	Confirm columns are in same order.

