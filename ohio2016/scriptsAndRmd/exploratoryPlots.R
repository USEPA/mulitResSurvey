# EXPLORATORY PLOTS

# GRTS ESTIMATES---------------
# Initial looks at emission rates
# pdf("ohio2016/output/figures/dirtySummary.pdf", paper = "a4r") # landscape orientation
# 

# PLOT LAKE SPECIFIC DATA
# Pull out whole lake data
meanVariance.c.lake <- filter(meanVariance.c, Subpopulation == "lake") 
fLake_Name <- factor(meanVariance.c.lake$Lake_Name,  # order Lake by CH4 emission ebullition rate
                     levels = meanVariance.c.lake[order(meanVariance.c.lake$ch4.trate.mg.h_Estimate), "Lake_Name"])
meanVariance.c.lake <- mutate(meanVariance.c.lake,  # incorporate ordered Lake_Name into data frame
                              fLake_Name = fLake_Name)

# Highlight Harsha Lake data with color
plotColor <- ifelse(meanVariance.c.lake$Lake_Name == "William H Harsha Lake", "red", "black")

ggplot(meanVariance.c.lake,
       aes(ebCh4mgM2h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ebCh4mgM2h_UCB95Pct, xmin = ebCh4mgM2h_LCB95Pct), color = plotColor)

ggplot(meanVariance.c.lake,
       aes(ch4.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.trate.mg.h_UCB95Pct, xmin = ch4.trate.mg.h_LCB95Pct), color = plotColor)

ggplot(meanVariance.c.lake,
       aes(ebCo2mgM2h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ebCo2mgM2h_UCB95Pct, xmin = ebCo2mgM2h_LCB95Pct), color = plotColor)


# 
# dev.off()