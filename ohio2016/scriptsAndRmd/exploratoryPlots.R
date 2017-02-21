# EXPLORATORY PLOTS

# Merge meanVariance.c with landuse and morphology data for descRes script.
meanVariance.c.lake.lu <- merge(filter(meanVariance.c,
                                       Subpopulation == "lake"),
                           descRes)

# GRTS ESTIMATES---------------
# Initial looks at emission rates

# DOT PLOT LAKE SPECIFIC DATA--------------
##---------------------------------------------------------------------------##
# Volumetric rates
# Highlight Harsha Lake data with color
plotColor <- ifelse(meanVariance.c.lake.lu$Lake_Name == "William H Harsha Lake", 
                    "red", "black")

# Set plotting order for volumetric emission rate
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "vol")
ggplot(meanVariance.c.lake.lu,
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
plotColor <- ifelse(meanVariance.c.lake.lu$Lake_Name == "William H Harsha Lake", "red", "black")

# Diffusive CH4  flux
# Reset plotting order for CH4 diffusion
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "ch4.d")
ggplot(meanVariance.c.lake.lu,
       aes(ch4.drate.mg.m2.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.drate.mg.m2.h_UCB95Pct, 
                     xmin = ch4.drate.mg.m2.h_LCB95Pct), color = plotColor)

ggsave('ohio2016/output/figures/ch4DifDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=6,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")


# Ebullition CH4 mass flux
# Reset plotting order for CH4 ebullition
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "ch4.e")
ggplot(meanVariance.c.lake.lu,
       aes(ch4.erate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.erate.mg.h_UCB95Pct, 
                     xmin = ch4.erate.mg.h_LCB95Pct), color = plotColor)

ggsave('ohio2016/output/figures/ch4EbDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=6,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# CH4 total rate
# Reset plotting order for CH4 ebullition
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "ch4.t")
ggplot(meanVariance.c.lake.lu,
       aes(ch4.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.trate.mg.h_UCB95Pct, 
                     xmin = ch4.trate.mg.h_LCB95Pct), 
                 color = plotColor) +
  xlab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  theme(axis.title.y = element_blank()) +  # Eliminate x-axis title
 ggtitle("Mean (95% CI) from grts function")

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
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "co2.d")
ggplot(meanVariance.c.lake.lu,
       aes(co2.drate.mg.m2.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.drate.mg.m2.h_UCB95Pct, 
                     xmin = co2.drate.mg.m2.h_LCB95Pct), color = plotColor)

# CO2 ebullition
# Reset plotting order for CO2 ebullition
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "co2.e")
ggplot(meanVariance.c.lake.lu,
       aes(co2.erate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.erate.mg.h_UCB95Pct, 
                     xmin = co2.erate.mg.h_LCB95Pct), 
                 color = plotColor)

# CO2 total rate
# Reset plotting order for CO2 ebullition
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "co2.t")
ggplot(meanVariance.c.lake.lu,
       aes(co2.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.trate.mg.h_UCB95Pct, xmin = co2.trate.mg.h_LCB95Pct), color = plotColor)


##---------------------------------------------------------------------------##
# Chlorophyll
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "chl")
ggplot(meanVariance.c.lake.lu,
       aes(chla_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = chla_UCB95Pct, xmin = chla_LCB95Pct), color = plotColor) +
  xlab(expression(chl~a~{mu}*g~L^{-1})) +
  theme(axis.title.y = element_blank())

ggsave('ohio2016/output/figures/chlaDotPlot.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# CORRELATIONS----------------------------
# Volumetric emissions by land use
m <- lm(ebMlHrM2_Estimate ~ percent.agg.ag, # univariate model
        data = meanVariance.c.lake.lu)
# Set up 
eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                 list(a = format(coef(m)[1], digits = 4), 
                      b = format(coef(m)[2], digits = 4), 
                      r2 = format(summary(m)$r.squared, digits = 3)))

dftext <- data.frame(percent.agg.ag = 30, 
                     ebMlHrM2_Estimate = 60, 
                     eq = as.character(as.expression(eq)))

ggplot(meanVariance.c.lake.lu,
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
ggplot(meanVariance.c.lake.lu,
       aes(percent.agg.ag, co2.trate.mg.h_Estimate)) +
  geom_point()

# CH4 total vs land use, excluding Acton, Alum, and Cowan
ggplot(filter(meanVariance.c.lake.lu, 
              !(Lake_Name %in% c("Acton Lake", "Alum Creek Lake", "Cowan Lake"))),
       aes(percent.agg.ag, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("% agricultural land use in watershed")

# CH4 total vs land use
ggplot(meanVariance.c.lake.lu,
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
ggplot(meanVariance.c.lake.lu,
       aes(mean.depth.m.morpho, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(Total~CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("mean depth (m)")

ggsave('ohio2016/output/figures/ch4TotbyDepth.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")



# CH4 total vs watershed:reservoir
ggplot(meanVariance.c.lake.lu,
       aes(rda, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("Watershed:reservoir area")

ggsave('ohio2016/output/figures/ch4TotbyRDA.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")




# CH4 total vs TP
ggplot(meanVariance.c.lake.lu,
       aes(tp_Estimate, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("TP (ug/L") 
  #coord_trans(x="log", y="log")




# CH4 total vs TN
ggplot(meanVariance.c.lake.lu,
       aes(tn_Estimate, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("TN (ug/L")




# Trap CH4 concentration
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, 
                                               choice1 = "ch4.ppm.trap")
ggplot(meanVariance.c.lake.lu,
       aes((trap_ch4.ppm_Estimate/10000), fLake_Name)) +
  geom_point() +
  geom_errorbarh(aes(xmax = trap_ch4.ppm_UCB95Pct/10000, 
                     xmin = trap_ch4.ppm_LCB95Pct/10000)) +
  xlab(expression(bubble~CH[4]~content~('%'))) +
  theme(axis.title.y = element_blank())
  

ggsave('ohio2016/output/figures/ch4.ppmTrap.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")


# Trap CH4 by date
ggplot(meanVariance.c.lake.lu, aes(deplyDt, (trap_ch4.ppm_Estimate/10000))) +
  geom_point() +
  geom_text(aes(label = Lake_Name),
            hjust = 0, vjust = 0, size = 2)


# Trap CH4 by land use
ggplot(meanVariance.c.lake.lu,
       aes(percent.agg.ag, (trap_ch4.ppm_Estimate/10000))) +
  geom_point() +
  ylab(expression(bubble~CH[4]~content~('%'))) +
  xlab("percent agriculture")
 
ggsave('ohio2016/output/figures/ch4.ppmTrapByLU.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")
 

# Trap CH4 by ebullition
ggplot(meanVariance.c.lake.lu,
       aes(ebMlHrM2_Estimate, (trap_ch4.ppm_Estimate/10000))) +
  geom_point() +
  ylab(expression(bubble~CH[4]~content~('%'))) +
  xlab("bubble rate")


# Trap CH4 by volumetric ebullition 
ggplot(eqAreaData,
       aes(ebMlHrM2, (trap_ch4.ppm/10000))) +
  geom_point() +
  ylab(expression(bubble~CH[4]~content~('%'))) +
  xlab("bubble rate")


# Trap CH4 by volumetric ebullition, excluding Acton, Alum, Cowan
ggplot(filter(eqAreaData,
              !(Lake_Name %in% c("Acton Lake", "Alum Creek Lake", "Cowan Lake"))),
       aes(ebMlHrM2, (trap_ch4.ppm/10000))) +
  geom_point() +
  ylab(expression(bubble~CH[4]~content~('%'))) +
  xlab("bubble rate")

# Nutrients and chlorophyll
ggplot(meanVariance.c.lake.lu, aes(tp_Estimate, chla_Estimate)) +
  geom_point() +
  ylab(expression(chl~a~{mu}*g~L^{-1}))
