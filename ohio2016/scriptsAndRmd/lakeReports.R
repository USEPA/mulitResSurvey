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


# Highlight Harsha Lake (lake of interest) data with color
plotColor <- ifelse(meanVariance.c.lake.lu$Lake_Name == "William H Harsha Lake", "red", "black")

# total CH4 emissions copied directly from exploratoryPlots.R
# CH4 total emission rate
# Reset plotting order for CH4 total emissions
# "orderLake" is a function defined in the masterLibrary.R script
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
## work in progress. Goal: produce a table for each lake with water quality and 
## GHG emissions info, including data that was not in the plots. Need to isolate
## the relevant data for each lake, rename the column headings, and add units

# dplyr package 
actonTable<-filter(eqAreaData, Lake_Name == "Acton Lake", EvalStatus == "sampled") %>%
  select(siteID, LatSamp, LongSmp, stratum, wtrDpth,  #site identification  
         TN, TNH4, TNO2, TNO2-3, TP, TRP, chla.sample, pheo.sample,  #nutrient data
         chla_S, DO__L_S,  DOPrc_S,  ORP_S,  pH_S, SpCn__S, Tmp_C_S,  TrNTU_S, #shallow sonde data
         smDpthD, chla_D, DO__L_D, DOPrc_D, ORP_D, pH_D, SpCn__D, Tmp_C_D, TrNTU_D, #deep sonde data
         co2.trate.mg.h, n2o.erate.mg.h, ch4.erate.mg.h, ch4.trate.mg.h ) #ghg emission rates
          #"filter" command filters rows of interest. We're filtering the dataset for the sampled sites at a given lake
          #"select" command selects the columns we want to show. The order they are listed in the () is the order they will appear

actonColNames<-c("Site ID", "Latitude", "Longitude", "Location Type", "Site Depth (m)",
                 "Total N (units)", "NH4", "NO2", "NO2 + NO3", "Total P", "Total Reduced P", "Chl a", "Pheo",
                 "Shallow Chl a", "Shallow DO (mg/L)", "Shallow DO (%)", "Shallow ORP", "Shallow pH", "Shallow Specific Conductivity", "Shallow Temperature (C)", "Shallow Turbidity",
                 "'Deep' Sample Depth (m)", "Deep Chl a", "Deep DO (mg/L)", "Deep DO (%)", "Deep ORP", "Deep pH", "Deep Specific Conductivity", "Deep Temperature (C)", "Deep Turbidity",
                 "CO2 Emission Rate (mg/h)", "N2O Emission Rate (mg/h)", "CH4 Ebullition Rate (mg/h)", "Total CH4 Emission Rate (mg/h)")

write.table(actonTable, file = "actonTable.txt", sep = ",", quote = F, row.names = F, col.names = actonColNames)

str(actonColNames)

kable(actonTable)

str(actonTable)

