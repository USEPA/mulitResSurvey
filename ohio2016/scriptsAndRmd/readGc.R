

# READ GC DATA----------------
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
                  as.is=TRUE, skip=43)
gas.5 <- read.xls(paste(rootDir, "Ebul_Dgas_16_07_13_STD_UNK.xlsx", sep=""),
                  as.is=TRUE, skip=36)
gas.6 <- read.xls(paste(rootDir, "Ebul_Dgas_16_08_31_STD_UNK.xlsx", sep=""),
                  as.is=TRUE, skip=31)
gas.7 <- read.xls(paste(rootDir, "Ebullition_16_10_12_STD_UNK.xlsx", sep=""),
                  as.is=TRUE, skip=46)
gas.8 <- read.xls(paste(rootDir, "Ebul_Trap_6ml_16_10_20_UNK_STD.xlsx", sep = ""),
                  as.is = TRUE, skip = 46)

# Need to strip a few samples before all are merged.
# These are floating chamber gas samples collected manually at Cowan Lake.  Were
# run with L_N2 samples, but need to be rerun.  Strip out here so code won't break
# when I read in spreadsheet with rerun results.
gas.5 <- filter(gas.5, !(Sample %in% c(16126:16129, 16133:16136)))

# Merge gas data.
gas.all <- Reduce(function(...) merge(..., all=T), list(gas.1, gas.2, gas.3, gas.4, gas.5, gas.6, gas.7, gas.8))  

# Need to make a few fixes before exetainer code is converted to integer
# The labels were lost from several Kiser Lake samples.  These samples were
# run on the GC as "unkgas1001" through "unkgas10012".  I was unable to
# figure out what samples they were, therefore removing from data set.
logicalIndicator <- !(grepl(pattern = "unkgas", x = gas.all$Sample)) #sample code with"unkgas"
gas.all <- gas.all[logicalIndicator, ]  # remove unkgas samples

# Strip out a few 2015 samples that were included with the cryo run
# from the 6 mL vials.
gas.all <- filter(gas.all, !Sample %in% c("2015 T 104", "2015 T919", "2015 T102"))


# Format gas data
gas.all <- select(gas.all, select = -Sample.code, -Sample.abb, # Exlcude variables
                  -Sample.date,
                  -Area.CO2, -Area.Methane, -Area.CO2.1, -Area.Methane.1,
                  -Area.N2O, 
                  -N2O.chk, -CO2.chk, -CH4.chk,
                  -Area.O2, -Area.Ar, -Area.N2,
                  -Ar.chk, -N2.chk, -O2.chk,
                  -X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9)  %>%
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



# Check for duplicates.  Should be none.
# Need to follow up on 16242
filter(gas.all, duplicated(sample,fromLast = TRUE) | duplicated(sample,fromLast = FALSE)) %>% arrange(sample)

# A few fixes to be made upstream of merge with xtrCodes
gas.all[gas.all$sample == 16023 & !is.na(gas.all$sample), "sample"] = 16230  # sample entered incorrectly in sequence

# PREPARE EXETAINER CODES----------------------
# Extract from eqAreaData
xtrCodes <- filter(eqAreaData, EvalStatus == "sampled") %>%
  select(Lake_Name, siteID, ArExtnrs, DG_Extn, TrapExtn)

# Remove white space
xtrCodes[, c("ArExtnrs", "DG_Extn", "TrapExtn")] <- apply(X = xtrCodes[, c("ArExtnrs", "DG_Extn", "TrapExtn")],
                                                         MARGIN = 2, 
                                                         function(x) gsub(x, pattern = " ", replacement = ""))

# Split codes into separate fields
xtrCodes <- separate(xtrCodes, ArExtnrs, into = c("ar.xtr.1", "ar.xtr.2", "ar.xtr.3"), sep = ",") %>%
  separate(DG_Extn, into = c("dg.xtr.1", "dg.xtr.2", "dg.xtr.3"), sep = ",") %>%
  separate(TrapExtn, into = c("tp.xtr.1", "tp.xtr.2", "tp.xtr.3"), sep = ",")

# Melt  
xtrCodes.m <- melt(xtrCodes, id.vars = c("Lake_Name", "siteID")) %>% # melt, converts exetainer code to factor
  mutate(value = as.integer(as.character(value))) %>%  # Must got from factor -->character-->integer
  mutate(variable = as.character(variable)) %>% # Must got from factor -->character
  filter(!is.na(value))  # remove NAs

# Simplify variable names
xtrCodes.m[grepl(pattern = ".1|.2|.3", x = xtrCodes.m$variable), "variable"] <- 
  gsub(pattern = ".1|.2|.3", replacement = "", x = xtrCodes.m[grepl(pattern = ".1|.2|.3", x = xtrCodes.m$variable), "variable"])

# Removing a few from Kiser Lake that were lost due to label falling off  (see above)
xtrCodes.m <- filter(xtrCodes.m, !(value %in% c(16236,16240,16255,16258,16252,16247,16263,16257,16244,16243)))

# Check for duplicates.  Should be none.
filter(xtrCodes.m, duplicated(value,fromLast = TRUE) | duplicated(value,fromLast = FALSE)) %>% arrange(value)
# Duplicate entries for 16882. This code was run and is clearly a trap sample.
# Omit ar.xtr SU-50 Cave Run should be omitted.
logicalIndicator <- with(xtrCodes.m, Lake_Name == "Cave Run Lake" & 
                           siteID == "SU-50" & 
                           variable == "ar.xtr" &
                           value == 16882)

xtrCodes.m <- filter(xtrCodes.m, !logicalIndicator)

# MERGE EXETAINER CODES WITH GC DATA-----

xtrCodes.gas <- merge(xtrCodes.m, gas.all, by.x = "value", by.y = "sample", all = TRUE)

str(xtrCodes.m)  #980 observations
str(gas.all) # 901 observations
str(xtrCodes.gas) # 1003 observations

# Specific fixes
# Still need to add codes for MIT trap redeployment

omitCodes <- c(16170, # run on GC, but field notes indicate is bad and not entered in field sheets
               16189, # 16189 air sample run w/trap samples.  Discard.
               16199, 16200, # contaminated, per field sheets
               16206, # bad, per field data sheets
               16023, # chromatogram overwritten due to sequence problem
               16298, 16299, # contaminated, omit, per field sheets.
               161235, 161281, 161257, 161262, 161266, 161258, # Cowan Lake cntl traps
               161237, 16151, 161265, 16165, 161279, 16158, # Cowan Lake cntl traps
               161267, 161268, 161269, # Harsha Lake MIT redeployment
               16242, 16143, 161244, # Caesar Cr. MIT redeployment
               16576,  # Empty short tube run on GC
               16603, # trap sample, but no record in field sheets.
               16825, # no record in field sheets
               16070) # Karen noted loose cap. Came from trap, but looks like air.
               


xtrCodes.gas <- filter(xtrCodes.gas, !(value %in% omitCodes))


                       
# Sample run on GC, but not in data sheets
filter(xtrCodes.gas, is.na(Lake_Name)) %>% arrange(value)  # 0


# Samples in data sheets, but GC data not yet read into R
filter(xtrCodes.gas, is.na(xtrCodes.gas$co2.ppm)) %>% arrange(variable, value)  # Many, but still analyzing GC samples


# QA/QC GC REPS--------------

pdf("ohio2016/output/figures/scatterplot3dTrap.pdf",
    paper = "a4r", width = 11, height = 8)  # initiate landscape pdf file)
par(mfrow = c(1,2))

uniqueCases <- filter(xtrCodes.gas, variable == "tp.xtr", # trap sample
                       !is.na(ch4.ppm), # has GC data
                       !is.na(Lake_Name)) %>% # is connected with Lake and station
  distinct(Lake_Name, siteID) # unique combinations of lake and site

for(i in 1:length(uniqueCases$Lake_Name)) {
  site.i <- uniqueCases$siteID[i]
  lake.i <- uniqueCases$Lake_Name[i]
  data.i <- filter(xtrCodes.gas, 
                   siteID == site.i, Lake_Name == lake.i, 
                   !is.na(ch4.ppm), variable == "tp.xtr")
  
  # CO2, CH4, N2 scatterplot
  try(
    with(data.i, {
      
      s3d <- scatterplot3d(co2.ppm/10000, ch4.ppm/10000, n2, 
                           xlab = "CO2 (%)", ylab = "CH4 (%)", zlab = "N2 (%)",
                           pch=21, bg = "red", main = uniqueCases[i, ])
      
      s3d.coords <- s3d$xyz.convert(co2.ppm/10000, ch4.ppm/10000, n2)
      text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
           labels=value,               # text to plot
           cex=.5, pos=4)           # shrink text 50% and place to right of points)
    }),
    silent = TRUE)
  
  # n2o, o2, ar scatterplot 
  try(
    with(data.i, {
      
      s3d <- scatterplot3d(n2o.ppm, o2, ar, 
                           xlab = "N2O (ppm)", ylab = "O2 (%)", zlab = "ar (%)",
                           pch=21, bg = "red", main = uniqueCases[i, ])
      
      s3d.coords <- s3d$xyz.convert(n2o.ppm, o2, ar)
      text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
           labels=value,               # text to plot
           cex=.5, pos=4)           # shrink text 50% and place to right of points)
    }),
    silent = TRUE)  
  
}
dev.off()

# Aggregate by Lake_Name and siteID, for now

xtrCodes.gas.g <- filter(xtrCodes.gas,
                              !is.na(ch4.ppm), # has GC data
                              !is.na(Lake_Name)) %>% # has lake and siteID
                              group_by(Lake_Name, siteID, variable) # group for aggregation

xtrCodes.gas.agg <- summarise(xtrCodes.gas.g, 
                     n2o.sd=sd(n2o.ppm, na.rm=TRUE),
                     m.n2o.ppm=mean(n2o.ppm, na.rm=TRUE),
                     n2o.cv= (n2o.sd/m.n2o.ppm) * 100,
                     
                     co2.sd=sd(co2.ppm, na.rm=TRUE),
                     m.co2.ppm=mean(co2.ppm, na.rm=TRUE),
                     co2.cv=(co2.sd/m.co2.ppm) * 100,
                     
                     ch4.sd=sd(ch4.ppm, na.rm=TRUE),
                     m.ch4.ppm=mean(ch4.ppm, na.rm=TRUE),
                     ch4.cv=(ch4.sd/m.ch4.ppm) * 100,                     
                     
                     o2.sd=sd(o2, na.rm=TRUE),
                     m.o2=mean(o2, na.rm=TRUE),
                     o2.cv=(o2.sd/m.o2) * 100,
                     
                     ar.sd=sd(ar, na.rm=TRUE),
                     m.ar=mean(ar, na.rm=TRUE),
                     ar.cv=(ar.sd/m.ar) * 100,
                     
                     n2.sd=sd(n2, na.rm=TRUE),
                     m.n2=mean(n2, na.rm=TRUE),
                     n2.cv=(n2.sd/m.n2) * 100) %>%
  rename(n2o.ppm = m.n2o.ppm, co2.ppm = m.co2.ppm, ch4.ppm = m.ch4.ppm,
         o2 = m.o2, ar = m.ar, n2 = m.n2) %>%
  mutate(total = (ch4.ppm/10000) + (co2.ppm/10000) + (n2o.ppm/10000) + n2 + o2 + ar)

xtrCodes.gas.agg <- ungroup(xtrCodes.gas.agg)  # This removes grouping, which complicates things down the line.

ggplot(xtrCodes.gas.agg, aes(siteID, ch4.ppm)) + # Everything appears to have agg correctly
  geom_point() +
  facet_grid(~variable, scales="free_y")   # lot of low CH4 trap values to look into

# MERGE RAW GC DATA WITH eqAreaData---------------
# Only merge air and trap data now.  Need to push dg through
# headspace equilibration calcs before using.
# 1) Need to melt, which requires a data.frame, not a dplyr tbl_df.
# 2) melt creates a 'variable' column, already have 'variable' column
# in xtrCodes.gas.agg. Must rename first.
xtrCodes.gas.agg <- rename(xtrCodes.gas.agg, type = variable) # rename 'variable'

xtrCodes.gas.agg.m <- melt(as.data.frame(xtrCodes.gas.agg), # convert tbl_df to df
id.vars = c("Lake_Name", "siteID", "type")) # specify id variable

xtrCodes.gas.agg.m <- mutate(xtrCodes.gas.agg.m, type =  # adopt more intuitive names
                             ifelse(type == "tp.xtr", "trap",
                                    ifelse(type == "ar.xtr", "air", type)))
  
xtrCodes.gas.agg.c <- dcast(filter(xtrCodes.gas.agg.m, type != "dg.xtr"), # cast
                            Lake_Name + siteID ~ type + variable) %>%
  select(-air_o2.sd, -air_o2, -air_o2.cv, -air_ar.sd, -air_ar, -air_ar.cv, -air_n2.sd,
         -air_n2, -air_n2.cv, -air_total)

# Merge
eqAreaData <- merge(xtrCodes.gas.agg.c, eqAreaData, all = TRUE)

