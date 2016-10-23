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
    select(Lake_Name, Subpopulation, Indicator, Estimate, LCB95Pct, UCB95Pct)  
})


# Coerce to df, format
meanVariance <- do.call("rbind", myMeanVarianceList)  # coerce to df
meanVariance[ , c("Subpopulation", "Indicator")] = apply(meanVariance[ , c("Subpopulation", "Indicator")], MARGIN = 2, FUN = as.character)


# Melt/dcast for plotting
meanVariance.m <- reshape2::melt(meanVariance)  # specify package.  reshape and reshape2 loaded
meanVariance.c <- dcast(meanVariance.m, formula = Lake_Name + Subpopulation ~ Indicator + variable) # cast

# Quick look at LU data
meanVariance.c.lu <- merge(mutate(meanVariance.c, lake.name = tolower(Lake_Name)), survRes, by.x = "lake.name", by.y = "lake.name")






# 3d Surface plots.  Won't run in R 3.3.0--------------------------

# a <- filter(meanVariance.c.lu, Subpopulation == "lake") %>% select(percent.agg.ag)
# b <-filter(meanVariance.c.lu, Subpopulation == "lake") %>% select(depth)
# z <- filter(meanVariance.c.lu, Subpopulation == "lake") %>% select(ebMlHrM2_Estimate)


# library not available in R 3.3.0
#persp3D(x=a,y=d,z,phi=15,theta=-30,main = "Hypothesized Surface", xlab = "Ag %", ylab = "Max Depth",
#        zlab = "Methane Emission Rate (mg/m^2/h)")
