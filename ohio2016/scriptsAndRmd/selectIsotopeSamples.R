# NEED TO SELECT BUBBLE GAS SAMPLES TO SEND OUT FOR ISOTOPE ANALYSIS
# WILL PRIORITIZE LAKES/SITES WHERE WE COLLECTED SEDIMENT FOR MEGAN

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

# Filter sites where we have mud, but no gas sample
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
write.table(filter(eqAreaData, !is.na(TrapExtn)) %>% select(Lake_Name, TrapExtn),
            "ohio2016/output/exetInventory.txt", row.names = FALSE, sep = "\t")
