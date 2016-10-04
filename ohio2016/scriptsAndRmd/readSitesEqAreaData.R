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

# Omit Acton, which is currently screwed up
fileNames <- fileNames[!grepl("acton", fileNames)]


# 2.  Read in files
mylist <- list()  # Create an empty list to hold data

for (i in 1:length(fileNames)){  # for each file
  # read.dbf is in foreign and spsurvey.  The foreign version allows for
  # use of as.is argument.
  data.i <- foreign::read.dbf(paste(rootDir, fileNames[i], sep = ""),
                    as.is=TRUE)
  mylist[[i]] <- data.i
}


# 3. Strip column names that are not consisent (or necessary) across different .dbf
# files.  Inconsistency related to source of original GIS shapefile.

# Vector of columns to remove
columnsToRemove <- c("OBJECTID|Permanent_|FDate|Resolution|GNIS_ID|Elevation|ReachCode|FType|FCode|Connectivi|Issue_Type|Lake_Name_|Reservoir_|QC")
                                  
# remove columns from all dfs in list
mylist1 <- lapply(mylist, function(x) select(x, -matches(columnsToRemove))) # matches allows for multiple terms

# Some dfs in list have column name "GNIS_Name".  Rename to Lake_Name where it appears. 
# ;x needed to have function report whole df.
mylist2 <- lapply(mylist1, function(x) {names(x) <- sub("GNIS_Name", "Lake_Name", names(x));x})

# Add 'section' as column, if not already present.  This happens in equal area designs
mylist3 <- lapply(mylist2, function(x){
  if("section" %in% names(x))  # if 'section' already exists
    x  # then report original df
  else 
    cbind(x, section = NA) # if 'section' doesn't exist, report new column of NAs
})
                                          

# eqAreaData <- do.call("rbind", mylist)  # This coerces the list into a dataframe. Cool...



# 
# STRATEGY TO FACILITATE merge
# Buckhorn, Brookeville, Carr Cr, and Cave Run have 57 columns, compared to 53 or 54 from most other point shapefiles.  Extra columns are due to field included in geodatabase with original polygon shapefile.  
#                              1.	 Rename GNIS_Name in above = Lake_Name in others
#                              2.  omit Don’t need: "OBJECTID"   "Permanent_" "FDate"     
#                              [13] "Resolution"   "Elevation"  "ReachCode"  "FType"     
#                              [19] "FCode"      
#                              
#                              AND
#                              Most 53 column files contain stuff we don’t need: "Connectivi" "Issue_Type"
#                              [13] "Lake_Name_" "Reservoir_" "QC"       
#                              
#                              2.	 Rename deplyDate to deplyDt
#                              3.	Add ‘section’ to equal area designs
#                              5.	Confirm columns are in same order.

