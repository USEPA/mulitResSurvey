# EXPLORATORY PLOTS

# GRTS ESTIMATES---------------
# Initial looks at emission rates
# pdf("ohio2016/output/figures/dirtySummary.pdf", paper = "a4r") # landscape orientation
# 

# DOT PLOT LAKE SPECIFIC DATA--------------
# Pull out whole lake data
meanVariance.c.lake <- filter(meanVariance.c, Subpopulation == "lake")

##---------------------------------------------------------------------------##
# CH4 rates first
# Highlight Harsha Lake data with color
plotColor <- ifelse(meanVariance.c.lake$Lake_Name == "William H Harsha Lake", "red", "black")

# Diffusive CH4  flux
# Reset plotting order for CH4 diffusion
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "ch4.d")
ggplot(meanVariance.c.lake,
       aes(ch4.drate.mg.m2.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.drate.mg.m2.h_UCB95Pct, 
                     xmin = ch4.drate.mg.m2.h_LCB95Pct), color = plotColor)

# Ebullition CH4 mass flux
# Reset plotting order for CH4 ebullition
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "ch4.e")
ggplot(meanVariance.c.lake,
       aes(ch4.erate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.erate.mg.h_UCB95Pct, 
                     xmin = ch4.erate.mg.h_LCB95Pct), color = plotColor)

# CH4 total rate
# Reset plotting order for CH4 ebullition
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "ch4.t")
ggplot(meanVariance.c.lake,
       aes(ch4.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.trate.mg.h_UCB95Pct, xmin = ch4.trate.mg.h_LCB95Pct), color = plotColor)


##---------------------------------------------------------------------------##
# CO2 rates

# Diffusive CO2  flux
# Reset plotting order for CO2 diffusion
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "co2.d")
ggplot(meanVariance.c.lake,
       aes(co2.drate.mg.m2.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.drate.mg.m2.h_UCB95Pct, 
                     xmin = co2.drate.mg.m2.h_LCB95Pct), color = plotColor)

# CO2 ebullition
# Reset plotting order for CO2 ebullition
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "co2.e")
ggplot(meanVariance.c.lake,
       aes(co2.erate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.erate.mg.h_UCB95Pct, 
                     xmin = co2.erate.mg.h_LCB95Pct), 
                 color = plotColor)

# CO2 total rate
# Reset plotting order for CO2 ebullition
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "co2.t")
ggplot(meanVariance.c.lake,
       aes(co2.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.trate.mg.h_UCB95Pct, xmin = co2.trate.mg.h_LCB95Pct), color = plotColor)


# CORRELATIONS----------------------------
# Pull out lake specific data with land use
meanVariance.c.lu.lake <- filter(meanVariance.c.lu, 
                                 Subpopulation == "lake")
# CO2 total vs land use
ggplot(meanVariance.c.lu.lake,
       aes(percent.agg.ag, co2.trate.mg.h_Estimate)) +
  geom_point()

# CH4 total vs land use
ggplot(meanVariance.c.lu.lake,
       aes(percent.agg.ag, ch4.trate.mg.h_Estimate)) +
  geom_point()
