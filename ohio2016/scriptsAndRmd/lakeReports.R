# Lake reports should include the following

# 1.  Introductory text desribing study and sampling specifics for the lake.
# 2.  2D map of sampled sites.
# 3.  3D scatterplot of 1) total CH4 emissions, 2) chl a, and 3) DO
# 4.  Ordered dotplot of total CH4 emissions, TP, TN, and chl a.  
#     Lake of interest should be highlighted with a different color.
# 5.  Table of other water quality values at each site  
# 6.  EPA disclaimer

# Probably work up a separate .rmd file for each report.  knit out to .pdf.


####################################################################################
### Item #4: Ordered dotplots of total CH4 emissions, TP, TN, and chla
### Sarah diving into R via copy - paste
# added TP and TN to the orderLake main function in masterLibrary
# choice1 = "TP" for phosphorus and "TN" for nitrogen

# total CH4 emissions copied directly from exploratoryPlots.R
# Highlight Harsha Lake (lake of interest) data with color
plotColor <- ifelse(meanVariance.c.lake.lu$Lake_Name == "William H Harsha Lake", "red", "black")

# CH4 total emission rate
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

# Chlorophyll 
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "chl")
ggplot(meanVariance.c.lake.lu,
       aes(chla_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = chla_UCB95Pct, xmin = chla_LCB95Pct), color = plotColor) +
  xlab(expression(chl~a~{mu}*g~L^{-1})) +
  theme(axis.title.y = element_blank())+
  ggtitle("Mean Chlorophyll A (95% CI)")

ggsave('ohio2016/output/figures/chlaDotPlot.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# TP 
# Reset plotting order to rank by TP
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "TP")
ggplot(meanVariance.c.lake.lu,
       aes(tp_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = tp_UCB95Pct, 
                     xmin = tp_LCB95Pct), 
                 color = plotColor) +
  xlab(expression(Total~Phosphorus~(units))) +   #NEED TO EDIT WITH UNITS
  theme(axis.title.y = element_blank()) +  # Eliminate x-axis title
  ggtitle("Mean TP (95% CI)")

ggsave('ohio2016/output/figures/TPDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=6,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# TN 
# Reset plotting order to rank by TN
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "TN")
ggplot(meanVariance.c.lake.lu,
       aes(tn_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = tn_UCB95Pct, 
                     xmin = tn_LCB95Pct), 
                 color = plotColor) +
  xlab(expression(Total~Nitrogen~(units))) + #NEED TO EDIT WITH UNITS
  theme(axis.title.y = element_blank()) +  # Eliminate x-axis title
  ggtitle("Mean TN (95% CI)")

ggsave('ohio2016/output/figures/TNDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=6,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

######################################################################
#### Item #5: Table of Water Quality Values by Site
## work in progress

#define object for the table
columnsToRemove <- c("air_n2o.sd|air_n2o.ppm|air_n2o.cv|air_co2.sd|air_co2.ppm|air_co2.cv|air_ch4.sd|air_ch4.ppm|air_ch4.cv|trap_o2.sd|trap_o2|trap_o2.cv|trap_ar.sd|trap_ar|trap_n2.sd|trap_n2|trap_n2.cv|trap_total|Area_km2|BrPrssr|chm_vol")

#Lake_Name|ArExtnrs|bbblngO|chmStTm|deplyDt|deplyTm|DG_Extn|EvalReason
# remove columns from all dfs in list
mylist1 <- lapply(mylist, function(x) select(x, -matches(columnsToRemove))) # ma


actonTable<-eqAreaData[eqAreaData$Lake_Name == "Acton Lake" &
                          eqAreaData$EvalStatus == "sampled", 2:8]





kable(actonTable)

str(actonTable)

