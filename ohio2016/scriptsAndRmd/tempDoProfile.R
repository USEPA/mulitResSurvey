# READ DEPTH PROFILE DATA-----------

rootDir <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/data/"

tempDo <- read_excel(paste(rootDir, "depthProfileData.xlsx", sep =""), 
                     sheet = "data") %>%
  filter(!is.na(Lake_Name))

# simplify names
names(tempDo) = gsub(pattern = c("\\(| |#|%|)|/"), replacement = ".", x = names(tempDo)) %>%
  tolower() 

# rename according to conventions in rLakeAnalyzer package, which I
# ended up not using here!
tempDo <- dplyr::rename(tempDo, # needed to specify package for rename
                        wtr = temp.c, 
                        doobs = do...sat.,
                        depth.m = sample.depth..m.) %>%
  select(lake_name, wtr, doobs, depth.m)


# CALCULATE DEPTH TO THERMOCLINE--------------
# Calculate water temp rate of change
tempDo <- 
  group_by(tempDo, lake_name) %>%
  mutate(wtrChange = (wtr - lag(wtr, 1)) / (depth.m - lag(depth.m, 1)))


# Function to calculate thermocline depth.
# Interpolates between measurement depths
meanThermoDepth <- function(df) {
  minCriteria <-  min(df$wtrChange, na.rm = TRUE)  # rate of change must be >= 1C/m
  if (minCriteria > -1) {maxDepth <- NA}
  if (minCriteria > -1) {minDepth <- NA}  
  if (minCriteria <= -1) {maxDepth <- df[which.min(df$wtrChange), "depth.m"]}
  if (minCriteria <= -1) {minDepth <- df[which.min(df$wtrChange) - 1, "depth.m"]}
  (minDepth + maxDepth) / 2
}

# Calculate thermocline depth
thermoDepth <- ddply(tempDo, .(lake_name), function(df) meanThermoDepth(df)) %>%
  rename(thermoDepth.m = V1)

# Print depth profile
pdf(file = 'ohio2016/output/figures/tempProfile.pdf')

for(i in 1:length(unique(tempDo$lake_name))) {
  reservoir.i = unique(tempDo$lake_name)[i]
  
  try(print(
    ggplot(tempDo[tempDo$lake_name == reservoir.i, ], aes(x=wtr, y=depth.m)) +   #aes_string helpful in for loop (http://stackoverflow.com/questions/13260626/selecting-data-frame-columns-to-plot-in-ggplot2)
      ggtitle(label = reservoir.i) +
      geom_point() +
      geom_hline(yintercept = thermoDepth[thermoDepth$lake_name == reservoir.i, "thermoDepth.m"]) +
      scale_y_reverse(), 
    silent=TRUE))
}
dev.off()


# CALCULATE DEPTH TO HYPOXIA (<10% DO)------------
hypoxicDepth <- filter(tempDo, row_number() == min(which(doobs < 10))) %>%
  select(lake_name, depth.m, doobs) %>%
  rename(hypoxicDepth.m = depth.m)

# Print do profile
pdf(file = 'ohio2016/output/figures/doProfile.pdf')

for(i in 1:length(unique(tempDo$lake_name))) {
  reservoir.i = unique(tempDo$lake_name)[i]
  
  try(print(
    ggplot(tempDo[tempDo$lake_name == reservoir.i, ], aes(x=doobs, y=depth.m)) +   #aes_string helpful in for loop (http://stackoverflow.com/questions/13260626/selecting-data-frame-columns-to-plot-in-ggplot2)
      ggtitle(label = reservoir.i) +
      geom_point() +
      geom_hline(yintercept = hypoxicDepth[hypoxicDepth$lake_name == reservoir.i, "hypoxicDepth.m"]$hypoxicDepth.m) +
      scale_y_reverse(), 
    silent=TRUE))
}
dev.off()

# MERGE THERMOCLINE AND HYPOXIA DEPTHS
# Merge depth to hypoxia and thermocline
hypoxicThermoDepths <- 
merge(select(hypoxicDepth, -doobs), thermoDepth, all = TRUE)

write.table(x = hypoxicThermoDepths, 
            file = "ohio2016/output/hypoxicAnoxicDepths.txt", 
            row.names = FALSE)
 