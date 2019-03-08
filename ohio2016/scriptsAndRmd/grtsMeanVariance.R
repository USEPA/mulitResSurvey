# SCRIPT FOR CALCULATING MEAN AND VARIANCE FROM GRTS DESIGN


# Loop to apply grtsMeanVariance function to each lake.
myMeanVarianceList <- list() # empty list to catch mean and variance

for (i in 1:length(unique(eqAreaData$Lake_Name))) {
  lake.i <- unique(eqAreaData$Lake_Name)[i]
  data.i <- filter(eqAreaData, Lake_Name == lake.i)
  
  myMeanVarianceList[[i]] <- grtsMeanVariance(data.i)  # this function is sourced from masterLibrary.R
  myMeanVarianceList[[i]]$Pct$Lake_Name = lake.i  # add lake name to dataframe!
}


# Extract portion of interest from list components  
myMeanVarianceList <- lapply(myMeanVarianceList, function(x) {  # apply function to each list element
  filter(x$Pct, Statistic == "Mean") %>%  # Pct is the portion we want
    select(Lake_Name, Subpopulation, Indicator, Estimate, LCB95Pct, UCB95Pct, StdError) %>%
    mutate(StdError = as.numeric(StdError)) # Comes out as a char of class "Asis"?
})


# Coerce to df, format
meanVariance <- do.call("rbind", myMeanVarianceList)  # coerce to df
meanVariance[ , c("Subpopulation", "Indicator")] = apply(meanVariance[ , c("Subpopulation", "Indicator")], MARGIN = 2, FUN = as.character)


# Melt/dcast for plotting
meanVariance.m <- reshape2::melt(meanVariance)  # specify package.  reshape and reshape2 loaded
meanVariance.c <- dcast(meanVariance.m, formula = Lake_Name + Subpopulation ~ Indicator + variable) # cast

# Add sample date to meanVariance.c
sample.dates <- select(eqAreaData, Lake_Name, deplyDt) %>% 
  distinct(Lake_Name, deplyDt) %>% 
  filter(!is.na(deplyDt))
meanVariance.c <- merge(meanVariance.c, sample.dates)


# Extract 'lake' scale data
meanVariance.c.lake <- filter(meanVariance.c, Subpopulation == "lake")

# In three lakes (Brookville|Buckhorn|Waynoka) we only have dissolved
# gas data from one site.  Sample from the other site was somehow lost.
# The function above reports NA for these lakes, which breaks the gbm.
# Substitute NA values with the measured value from one site in each of 
# these 3 lakes.

dgData <- filter(eqAreaData,  # Measurement made at each site.
                 grepl("Brook|Buckhorn|Waynoka", Lake_Name),
                 !is.na(dissolved.ch4)) %>%
  select(Lake_Name, dissolved.ch4, dissolved.co2)

meanVariance.c.lake  <- meanVariance.c.lake %>%
  mutate(dissolved.ch4_Estimate = ifelse(Lake_Name == "Lake Waynoka", # update CH4 for 3 lakes
                                         dgData[dgData$Lake_Name == "Lake Waynoka", "dissolved.ch4"],
                                         ifelse(Lake_Name == "Buckhorn Lake",
                                                dgData[dgData$Lake_Name == "Buckhorn Lake", "dissolved.ch4"],
                                                ifelse(Lake_Name == "Brookville Lake",
                                                       dgData[dgData$Lake_Name == "Brookville Lake", "dissolved.ch4"],
                                                       dissolved.ch4_Estimate))),
         dissolved.co2_Estimate = ifelse(Lake_Name == "Lake Waynoka", # update co2 for 3 lakes
                                         dgData[dgData$Lake_Name == "Lake Waynoka", "dissolved.co2"],
                                         ifelse(Lake_Name == "Buckhorn Lake",
                                                dgData[dgData$Lake_Name == "Buckhorn Lake", "dissolved.co2"],
                                                ifelse(Lake_Name == "Brookville Lake",
                                                       dgData[dgData$Lake_Name == "Brookville Lake", "dissolved.co2"],
                                                       dissolved.co2_Estimate))))
