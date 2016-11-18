# EXPLORATORY PLOTS

# GRTS ESTIMATES---------------
# Initial looks at emission rates

# DOT PLOT LAKE SPECIFIC DATA--------------
##---------------------------------------------------------------------------##
# Volumetric rates
# Highlight Harsha Lake data with color
plotColor <- ifelse(meanVariance.c.lu$Lake_Name == "William H Harsha Lake", 
                    "red", "black")

# Set plotting order for volumetric emission rate
meanVariance.c.lu$fLake_Name <- orderLake(meanVariance.c.lu, choice1 = "vol")
ggplot(meanVariance.c.lu,
       aes(ebMlHrM2_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ebMlHrM2_UCB95Pct, 
                     xmin = ebMlHrM2_LCB95Pct), 
                 color = plotColor)

ggsave('ohio2016/output/figures/ch4VolDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=6,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")
##---------------------------------------------------------------------------##
# CH4 rates first
# Highlight Harsha Lake data with color
plotColor <- ifelse(meanVariance.c.lu$Lake_Name == "William H Harsha Lake", "red", "black")

# Diffusive CH4  flux
# Reset plotting order for CH4 diffusion
meanVariance.c.lu$fLake_Name <- orderLake(meanVariance.c.lu, choice1 = "ch4.d")
ggplot(meanVariance.c.lu,
       aes(ch4.drate.mg.m2.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.drate.mg.m2.h_UCB95Pct, 
                     xmin = ch4.drate.mg.m2.h_LCB95Pct), color = plotColor)

# Ebullition CH4 mass flux
# Reset plotting order for CH4 ebullition
meanVariance.c.lu$fLake_Name <- orderLake(meanVariance.c.lu, choice1 = "ch4.e")
ggplot(meanVariance.c.lu,
       aes(ch4.erate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.erate.mg.h_UCB95Pct, 
                     xmin = ch4.erate.mg.h_LCB95Pct), color = plotColor)

# CH4 total rate
# Reset plotting order for CH4 ebullition
meanVariance.c.lu$fLake_Name <- orderLake(meanVariance.c.lu, choice1 = "ch4.t")
ggplot(meanVariance.c.lu,
       aes(ch4.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.trate.mg.h_UCB95Pct, 
                     xmin = ch4.trate.mg.h_LCB95Pct), 
                 color = plotColor) +
  xlab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  theme(axis.title.y = element_blank())  # Eliminate x-axis title

ggsave('ohio2016/output/figures/ch4TotDotChart.tiff',  # export as .tif
units="in",  # specify units for dimensions
width=6,   # 1 column
height=6, # Whatever works
dpi=600,   # ES&T. 300-600 at PLOS One,
compression = "lzw")


##---------------------------------------------------------------------------##
# CO2 rates

# Diffusive CO2  flux
# Reset plotting order for CO2 diffusion
meanVariance.c.lu$fLake_Name <- orderLake(meanVariance.c.lu, choice1 = "co2.d")
ggplot(meanVariance.c.lu,
       aes(co2.drate.mg.m2.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.drate.mg.m2.h_UCB95Pct, 
                     xmin = co2.drate.mg.m2.h_LCB95Pct), color = plotColor)

# CO2 ebullition
# Reset plotting order for CO2 ebullition
meanVariance.c.lu$fLake_Name <- orderLake(meanVariance.c.lu, choice1 = "co2.e")
ggplot(meanVariance.c.lu,
       aes(co2.erate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.erate.mg.h_UCB95Pct, 
                     xmin = co2.erate.mg.h_LCB95Pct), 
                 color = plotColor)

# CO2 total rate
# Reset plotting order for CO2 ebullition
meanVariance.c.lu$fLake_Name <- orderLake(meanVariance.c.lu, choice1 = "co2.t")
ggplot(meanVariance.c.lu,
       aes(co2.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.trate.mg.h_UCB95Pct, xmin = co2.trate.mg.h_LCB95Pct), color = plotColor)


# CORRELATIONS----------------------------
# Merge with landuse data.
# Need to adopt lake names consistent with meanVariance.c.lu
survRes$Lake_Name <- ifelse(survRes$lake.name == "BVR",
                            "brookville lake",
                            ifelse(survRes$lake.name == "BHR",
                                   "buckhorn lake",
                                   ifelse(survRes$lake.name == "ceaser creek lake",
                                          "caesar creek lake",
                                          ifelse(survRes$lake.name == "CFK",
                                                 "carr fork lake",
                                                 ifelse(survRes$lake.name == "CRR",
                                                        "cave run lake",
                                                        survRes$lake.name)))))

# Lake_Name in meanVariance.c.lu must be lowercase
meanVariance.c.lu <- merge(mutate(meanVariance.c.lu, 
                                      lLake_Name = tolower(Lake_Name)),
                               survRes, 
                           by.x = "lLake_Name",
                           by.y = "Lake_Name") %>%
  mutate(rda = watershed.area.m2 / reservoir.area.m2,
         si = res.perimeter.m / reservoir.area.m2)


# Volumetric emissions by land use
m <- lm(ebMlHrM2_Estimate ~ percent.agg.ag, # univariate model
        data = meanVariance.c.lu)
# Set up 
eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                 list(a = format(coef(m)[1], digits = 4), 
                      b = format(coef(m)[2], digits = 4), 
                      r2 = format(summary(m)$r.squared, digits = 3)))

dftext <- data.frame(percent.agg.ag = 30, 
                     ebMlHrM2_Estimate = 60, 
                     eq = as.character(as.expression(eq)))

ggplot(meanVariance.c.lu,
       aes(percent.agg.ag, ebMlHrM2_Estimate)) +
  geom_point() +
  # geom_errorbar(aes(ymax = ebMlHrM2_UCB95Pct,  
  #                   ymin = ebMlHrM2_LCB95Pct)) +
  geom_smooth(method = "lm", se = FALSE) +
  #geom_text(aes(label = eq), data = dftext, parse = TRUE) +
  ylab(expression(volumetric~flux~(mL~ m^{2}~ hr^{-1}))) +
  xlab("% agricultural land use in watershed")

ggsave('ohio2016/output/figures/volbyLU.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# CO2 total vs land use
ggplot(meanVariance.c.lu,
       aes(percent.agg.ag, co2.trate.mg.h_Estimate)) +
  geom_point()

# CH4 total vs land use
ggplot(meanVariance.c.lu,
       aes(percent.agg.ag, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("% agricultural land use in watershed")

ggsave('ohio2016/output/figures/ch4TotByLU.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# CH4 total vs depth
ggplot(meanVariance.c.lu,
       aes(max.depth.ft, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("maximum depth (ft)")

ggsave('ohio2016/output/figures/ch4byDepth.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# CH4 total vs watershed:reservoir
ggplot(meanVariance.c.lu,
       aes(rda, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("RDA")

# CH4 total vs TP
ggplot(meanVariance.c.lu,
       aes(tp_Estimate, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("TP (ug/L")

# CH4 total vs TN
ggplot(meanVariance.c.lu,
       aes(tn_Estimate, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("TN (ug/L")




