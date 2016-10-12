

# READ GC DATA
# Read individual files.  Had trouble with read_excel

rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/data/gcData/"

gas.1 <- read.xls(paste(rootDir, "Ebullition_16_06_10_STD_UNK_2016_samples.xlsx", 
                        sep =""), as.is=TRUE, skip=39) 
gas.2 <- read.xls(paste(rootDir, "Ebullition_16_08_09_STD_UNK.xlsx", sep = ""),
                  as.is=TRUE, skip=43)
# Can't seem to read in ""Ebullition_16_09_08_STD_UNK.xlsx".  I pasted
# the data into empty excel file, which is read in below.
gas.3 <- read.xls(paste(rootDir, "Ebullition_16_09_08_STD_UNK_Simplified.xlsx", sep = ""),
                  as.is=TRUE, skip = 0)
gas.4 <- read.xls(paste(rootDir, "Ebullition_16_09_20_STD_UNK.xlsx", sep=""),
                  as.is=TRUE, skip=28)

# Merge and format gas data.
gas.all <- Reduce(function(...) merge(..., all=T), list(gas.1, gas.2, gas.3, gas.4))  

gas.all <- select(gas.all, select = -Sample.code, -Sample.abb, # Exlcude variables
                  -Sample.date,
                  -Area.CO2, -Area.Methane, -Area.CO2.1, -Area.Methane.1,
                  -Area.N2O, 
                  -N2O.chk, -CO2.chk, -CH4.chk,
                  -Area.O2, -Area.Ar, -Area.N2,
                  -Ar.chk, -N2.chk, -O2.chk,
                  -X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9,
                  -X90.10.1, -X414.518, -X2.778.229, -X.10, -X.11, -X.12, -X.13,
                  -X.14, -X.15, -X.16, -X.17, -X.18, -X.19, -X.20, -X.21)  %>%
  filter(!(grepl("STD", gas.all$Sample)), # remove standards
         !(grepl("Std", gas.all$Sample)), # remove standards
         !(grepl("std", gas.all$Sample)), # remove standards
         Sample != "") %>%  # exclude blank rows
  rename(N2O.ppm = N2O..ppm., CO2.ppm = CO2..ppm., CH4.ppm = CH4..ppm.,
         O2 = O2...., Ar= Ar..., N2 = N2...)  %>%
  mutate(total = (CH4.ppm/10000) + (CO2.ppm/10000) + (N2O.ppm/10000) + N2 + O2 + Ar, 
         Sample = as.integer(Sample))  # convert to integer.  consistent with Exetainer codes

names(gas.all) = gsub(pattern = " ", replacement = ".", x = names(gas.all))
names(gas.all) = tolower(names(gas.all))



#Check for duplicates.  Should be none.
filter(gas.all, duplicated(sample,fromLast = TRUE) | duplicated(sample,fromLast = FALSE)) %>% arrange(sample)
```
