## SCRIPT FOR READING CHLOROPHYLL

# LIBRARY----------
# source("ohio2016/scriptsAndRmd/masterLibrary.R")

# READ IN DATA FILES DELIVERED FROM -------------------

# 1. Create a list of files to read in.  The completed data files all
# contain the pattern Beaulieu....xls and are stored in 
# L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/inputData/
# directory.

rootDir <- "C:/Users/JBEAULIE/GitRepository/mulitResSurvey/ohio2016/inputData/J. Beaulieu Chlorophyll 2016/"

fileNames <- list.files(path = rootDir, 
                        pattern = "Beaulieu", # file names containing this pattern
                        recursive = TRUE) # look in all subdirectories


# 2.  Read in files
mylist <- list()  # Create an empty list to hold data

for (i in 1:length(fileNames)){  # for each file
  data.i <- read_excel(paste(rootDir, fileNames[i], sep = ""), skip = 4)
  mylist[[i]] <- data.i
}

# 3.  Format elements of dataframe
# Rename sample ID column
mylist1 <- lapply(mylist, function(x) 
  {names(x) <- sub("Wavelength  nm", "sampleID", names(x));x})

# Append letters to begining of column names.  R doesn't like numeric names.
mylist2 <- lapply(mylist1, function(x) 
{names(x) <- paste("nm", names(x), sep = "");x})

mylist3 <- lapply(mylist2, function(x) x[-1, 1:6]) # remove first row, empty

chl <- do.call("rbind", mylist3)  # Coerces list into dataframe.

# Remove rows with all NA
chl <- chl[!is.na(chl)[,1], ]

# 3.  Manipulte data frame for chlorophyll calculation
# Need to create columns for acidified and unacidified absorbance.
# This is more difficult when samples are present that weren't
# read with and without acid.  This is the case for some QA/QC samples
# and a few unknowns.  strip out those weird samples first.

# Create column to classify observations as 'acidified' or 'unacidified'
chl <- mutate(chl, 
              acidified = ifelse(grepl(pattern = "-A", x = nmsampleID),
                                      "A",
                                      "U"))
# Remove QA/QC samples
chl <- filter(chl, !(nmsampleID %in% c("BS1", "BE1", "QCS1",
                                     "BS2", "BE2", "QCS2",
                                     "QCS", "QCS-1", "QCS-2")))

# Check to see if all remaining samples were run w/ + w/out acid.
# Strip "-A" from sampleID so unacadified and acidified samples can be grouped.
chl <- mutate(chl, nmsampleID = gsub(pattern = "-A", x = nmsampleID, 
                                replacement = ""))

# Identify samples that weren't run with and without acid.  Should have sample
# name present twice after -A removed (see above)
dupIndex <- duplicated(chl$nmsampleID, fromLast = FALSE) | 
  duplicated(chl$nmsampleID, fromLast = TRUE) 

# Melt after strippling tibble class and removing samples not run w/ and 
# w/out acid.
chl.m <- melt(as.data.frame(chl[dupIndex,])) # 'tibble' class screws up melt

# Cast to wide
chl.c <- dcast(data = chl.m, nmsampleID ~ variable + acidified )

# 4.  Calculate chlorophyll and pheophytin in material extracted from filter
# Using Lorenzen's pheopigment corrected. NERL 4460-9 and APHA 10-18
chl.c <- mutate(chl.c,
                nm664b = nm664_U - nm750_U,
                nm665a = nm665_A - nm750_A,
                chla.ug = (nm664b - nm665a)*26.7,
                pheo = ((1.7*nm665a) - nm664b)*26.7)   

# 5. Read in Karen's spreadsheet with filter volumes
chlVol <- read_excel("ohio2016/inputData/J. Beaulieu Chlorophyll 2016/chlorophyllVolume2016.xlsx",
                     sheet = "Ebullition")

# Strip unusual character names from column titles.
# The escape (\\) is needed to get R to treat them as character.
names(chlVol) = gsub(pattern = c("\\(| |#|)|/"), replacement = ".", x = names(chlVol))

# 6.  Make some fixes to data
chl.c <- mutate(chl.c, nmsampleID = ifelse(nmsampleID == "06302016.ATW.SUQ3.0.1.UNK",
                                  "06302016.ATW.SU03.0.1.UNK",
                                  ifelse(nmsampleID == "07112016.RFL.S-31.0.1.UNK",
                                          "07112016.RFL.S31.0.1.UNK",
                                         nmsampleID)))

# Are sample codes from Karen's data sheet in the chl data?
# All accounted for!
chlVol[!(chlVol$sample.ID.code %in% chl.c$nmsampleID), "sample.ID.code"]

# 7. Merge chl data with filter volume
chlFinal <- merge(select(chl.c, nmsampleID, chla.ug, pheo), 
                  select(chlVol, sample.ID.code, volume.filtered..mL.),
                  by.x = "nmsampleID", 
                  by.y = "sample.ID.code", 
                  all.y = TRUE)  # keep all obs from Karen's data sheet

# 8. Calculate chl a of original sample
# See APHA 10-18 for details.
# 5cm path length, 10mL extraction volume
chlFinal <- mutate(chlFinal,
                   # 1000000 convert from mL to m^3
                   # 5 is for 5cm path length
                   chla.sample = (chla.ug * 0.01) / ((volume.filtered..mL./1000000) * 5),
                   pheo.sample = pheo * 0.01 / ((volume.filtered..mL./1000000) * 5))


# 9. Separate calibration and regular samples
chlCal <- filter(chlFinal, grepl(pattern = "CAL", x = nmsampleID))
chlFinal <- filter(chlFinal, !grepl(pattern = "CAL", x = nmsampleID))

#10. Create unique identifiers for merge into eqAreaData
charIds <- strsplit(chlFinal$nmsampleID,split = "\\.") %>% # split on periods
  lapply(function(x) { # apply function to each element of list
    x[2:3] # extract second and third elements (lake name and site)
  }) %>%
  unlist()  # unlist into vector

# Coerce into df and format
charIds <- data.frame(Lake = charIds[seq(1, length(charIds), 2)], # extract lake
                      siteID = charIds[seq(2, length(charIds), 2)], # extract site
                      stringsAsFactors = FALSE) %>% 
  mutate(siteID = ifelse(siteID == 46,  # site SU-46 at Cave Run not coded right
                         "SU-46",
                  ifelse(siteID == 0, # site S-09 at Roaming not coded right
                          "S-09",
                  ifelse(siteID == "SU7", # BVR site not entered right
                          "SU-07",
                  ifelse(siteID == "U14",
                         "SU-14",
                  ifelse(!grepl("-", siteID) & nchar(siteID) == 4, # add "-"
                         paste(substr(siteID, 1,2), "-", substr(siteID,3,4), sep = ""),
                  ifelse(!grepl("-", siteID) & nchar(siteID) == 3, # add "-"
                        paste(substr(siteID, 1, 1), "-", substr(siteID,2, 3), sep = ""),
                        siteID)))))),
         # Bring in true lake name.  key is derived from masterLibrary
         Lake_Name = translationKeydf[match(Lake, translationKeydf$site), "Lake_Name"])

# 11. merge properly formatted siteID back into chlFinal.
chlFinal <- cbind(select(chlFinal, chla.sample, pheo.sample), 
                  select(charIds, Lake_Name, siteID))

# 12. merge chl data into eqAreaData
str(eqAreaData) #1426 observations
eqAreaData <- merge(chlFinal, eqAreaData, all = TRUE)
str(eqAreaData) # Still 1426, merged as expected

# 13. Read in chlorophyll measured in lab using data sonde
sondeChl <- read_excel(paste(rootDir, 
                             "sondeChlCalibrationChecks.xlsx", 
                             sep = "")) 

# Strip unusual character names from column titles.
# The escape (\\) is needed to get R to treat them as character.
names(sondeChl) = gsub(pattern = c("\\(| |#|)|/"), 
                       replacement = ".", 
                       x = names(sondeChl))
sondeChl <- mutate(sondeChl, FILTER.DATE = as.Date(FILTER.DATE))

# 14. Format lab based chl measurement of cal samples for merge
# Pull out Filter date from spec based chl measurements.
chlCal  <- mutate(chlCal, FILTER.DATE = substr(nmsampleID, 1, 8),
                  FILTER.DATE = as.Date(FILTER.DATE, 
                                        format = "%m%d%Y"))
chlCal <- merge(sondeChl, 
                select(chlCal, chla.sample, FILTER.DATE))
