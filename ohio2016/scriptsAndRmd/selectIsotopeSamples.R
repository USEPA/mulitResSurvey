# NEED TO SELECT BUBBLE GAS SAMPLES TO SEND OUT FOR ISOTOPE ANALYSIS
# WILL PRIORITIZE LAKES/SITES WHERE WE COLLECTED SEDIMENT FOR MEGAN

# FIGURE OUT WHAT TRAP GAS SAMPLES CORRELATE WITH SITES WHERE MEGAN----------
# COLLECTED SEDIMENT SAMPLES

# Read in list of sediment samples given to Megan
sedList <- read_excel("Copy of Multi Reservoir Sediment Sample List.xlsx", 
                      skip =2)

# Clean up names
names(sedList) = gsub(pattern = c("\\(| |#|)|/|-|\\+"), 
                      replacement = ".", x = names(sedList)) %>%
  tolower()

# Remove junk data                                                   
sedList <- filter(sedList, !is.na(site)) %>%
  select(reservoir, site)

# Merge with other site specific data
sedListVol <- merge(sedList, 
                    select(eqAreaData, Lake_Name, siteID, TrapExtn),
                    by.x = c("reservoir", "site"),
                    by.y = c("Lake_Name", "siteID"),
                    all.x = TRUE)

# Filter sites where we have mud, but no trap-gas sample
# Compare site location against map, attemp to id good substitue site
filter(sedListVol, is.na(TrapExtn))

# Record sub site, if available
# Lake Name, Megan site, substitute site, substitue exetainer
subList <- 
c(
"Atwood Lake", "SU-33", "SU-36", "16565", #could use SU-31
"Atwood Lake", "SU-03", NA, NA, #no good match
"Carr Fork Lake", "SU-04", NA, NA, #no match
"Carr Fork Lake", "SU-29", "SU-33", "16589",
"Lake Milton", "SU-34", NA, NA, #no match
"Lake Mohawk", "SU-28", "S-33", "16818",
"Lake Waynoka", "SU-05", NA, NA, #no match
"Michael J Kirwan Reservoir", "S-04", NA, NA, #no match
"Senecaville Lake", "S-21", NA, NA) #no match

# coerce vector into df
subDf <- data.frame(reservoir = subList[seq(1, length(subList), 4)],
                    site = subList[seq(2, length(subList), 4)],
                    subSite = subList[seq(3, length(subList), 4)], # substitute site
                    subExtn = subList[seq(4, length(subList), 4)], # substitute extn
                    stringsAsFactors = FALSE)

# merge substitute sites into original site list
sedListVolSub <- merge(sedListVol, subDf,
                         all= TRUE)

# Add exetainer codes for substitute sites
sedListVolSub <- mutate(sedListVolSub,
                        useSite = ifelse(!is.na(subSite), # site to use
                                         subSite,
                                         site),
                        useExtn = ifelse(!is.na(subExtn), # extn to use
                                         subExtn,
                                         TrapExtn)) %>%
  filter(!is.na(useExtn))  # extract sites with no sample available.

# Number of sites.  We can send out up 96 samples.
length(sedListVolSub$reservoir) #27 sites/samples

# Merge with lake specific data
sedListVolSubChl <- merge(sedListVolSub,
                      select(meanVariance.c.lake.lu, Lake_Name, chla_Estimate,
                             percent.cultivated.crops),
                      by.x = "reservoir", by.y = "Lake_Name", all.x = TRUE)


# Plot to see how subset lakes span chl a gradient
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "chl")
ggplot(meanVariance.c.lake.lu, aes(chla_Estimate, fLake_Name)) +
  geom_point(aes(color = ifelse(meanVariance.c.lake.lu$Lake_Name %in% 
                                  unique(sedListVolSub$reservoir), "yes", "no"))) +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position="none")

# Produce list of samples to see what we still have here.
#write.table(filter(eqAreaData, !is.na(TrapExtn)) %>% select(Lake_Name, TrapExtn),
#            "ohio2016/output/exetInventory.txt", row.names = FALSE, sep = "\t")


# COMPARE LIST OF GAS SAMPLES THAT WE WOULD LIKE TO RUN, AGAINST LIST OF EXETAINERS---------
# THAT WE STILL HAVE IN BUILDING

# sedListVolSub$useExtn is a character vector, often containing multiple
# exetainer codes (i.e. "16606, 16607, 16608").  These need to be broken into 
# components and converted to numeric.

# Split each row into an element of a list
# http://stackoverflow.com/questions/8464312/convert-comma-separated-entry-to-columns
splitDat <- do.call("rbind", strsplit(sedListVolSubChl$useExtn, ",")) 
# Convert to df, coerce to numeric
# This recycles values from within a row, therefore rows that don't have
# 3 exetainer codes will be populated with duplicates, that should be 
# removed
splitDat <- data.frame(apply(splitDat, 2, as.numeric))
names(splitDat) = c("useXtr1", "useXtr2", "useXtr3")
foo <- apply(splitDat, 1, duplicated) # ID duplicates
foo <- data.frame(Extn1 = foo[1,], # Get logical back into proper order
                 Extn2 = foo[2,],
                 Extn3 = foo[3,])
splitDat[as.matrix(foo)] <-  NA # set dups to NA

# Add separated exetainer codes into sedListVolSubChl
sedListVolSubChl <- cbind(sedListVolSubChl, splitDat)


# Read in inventory of remaining gas samples
# Add 16 to all extenainer codes.
exetInv <- read_excel("ohio2016/misc/exetList.xlsx") %>%
  mutate(exetainer_number = as.numeric(paste("16", exetainer_number, sep = "")))

sedListVolSubChl <- mutate(sedListVolSubChl,
                           useXtr1 = ifelse(useXtr1 %in% exetInv$exetainer_number,
                                            useXtr1,
                                            NA),
                           useXtr2 = ifelse(useXtr2 %in% exetInv$exetainer_number,
                                            useXtr2,
                                            NA),
                           useXtr3 = ifelse(useXtr3 %in% exetInv$exetainer_number,
                                            useXtr3,
                                            NA)
                           )

# THE AFTERMATH------------------

# How many lakes for which our inventory contains at least one trap sample collected 
# at or near Megans mud sample.
filter(sedListVolSubChl, !is.na(useXtr1) | !is.na(useXtr2) | !is.na(useXtr3)) %>%
  summarise(num_reservoirs = length(unique(reservoir)),
            num_sites = length(site))  # Samples from 14 reservoirs

# Total number of samples
sum(is.na(sedListVolSubChl[, c("useXtr1", "useXtr2", "useXtr3")])) # 31 samples

# Add gas composition data
# Simplify and melt
isoSamplComp <- select(sedListVolSubChl, reservoir, site, 
                           useSite, useXtr1, useXtr2, useXtr3) %>%
  melt() %>% rename(sample.code = value) %>%
  filter(!is.na(sample.code)) %>%
  left_join(., gas.all, by = c("sample.code" = "sample")) %>%
  select(reservoir, site, useSite, sample.code, n2o.ppm,
         co2.ppm, ch4.ppm)
 

write.table(isoSamplComp,
            "ohio2016/output/trap13ch4SamplesMegan.txt", 
            row.names = FALSE, sep = "\t")

# Plot to see how subset lakes span chl a gradient
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "chl")
ggplot(meanVariance.c.lake.lu, aes(chla_Estimate, fLake_Name)) +
  geom_point(aes(color = ifelse(meanVariance.c.lake.lu$Lake_Name %in% 
                                  with(sedListVolSubChl,
                                       sedListVolSubChl[!is.na(useXtr1) | !is.na(useXtr2) | !is.na(useXtr3) , "reservoir"]),
                                "yes", "no"))) +
  scale_color_manual("Sample available", values = c("black", "red")) +
  theme(axis.title.y = element_blank())

ggsave("ohio2016/output/figures/trapCh4IsotopeLakesForMegan.tiff",
       units="in",  # specify units for dimensions
       width=6,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

