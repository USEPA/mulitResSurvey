# EFFORT TO REPRODUCE BASTVIKEN 2004 AND RINTA 2017


# Data---------------
# Bastviken et al 2004 data
bastDat <- read_excel("ohio2016/inputData/literatureData/Data table Bastviken et al 2014 with Chl and pH.xlsx",
                      sheet = "formattedForSynthesisStudy")

names(bastDat) = gsub(pattern = c("\\(| |#|)|/|-|\\+"), replacement = ".", 
                     x = names(bastDat))

# add 'Type'  and 'Source' columns
bastDat <- mutate(bastDat, Source = "Bastviken et al. 2004")

# Remove empty columns (none present here). If a column is empty in one df,
# but populated in another, read_excel can assign them different structures (i.e.
# character vs numeric), which breaks the full_join call.  Best to simply remove
# empty columns and let full_join reconcile the differences.
bastDat <- bastDat[, !apply(is.na(bastDat), 2, all)]  


# Rinta 2017 et al.
rintDat <- read_excel("ohio2016/inputData/literatureData/rintaEtAl2017si.xlsx",
                      sheet = "formattedForSynthesisStudy")
names(rintDat) = gsub(pattern = c("\\(| |#|)|/|-|\\+"), replacement = ".", 
                      x = names(rintDat))

# add 'Type'  and 'Source' columns
rintDat <- mutate(rintDat, Source = "Rinta et al. 2017")

# Remove empty columns (none present here). If a column is empty in one df,
# but populated in another, read_excel can assign them different structures (i.e.
# character vs numeric), which breaks the full_join call.  Best to simply remove
# empty columns and let full_join reconcile the differences.
rintDat <- rintDat[, !apply(is.na(rintDat), 2, all)]


# Deemer 2016 et al.
deemDat <- read_excel("ohio2016/inputData/literatureData/Supplementary_Reservoir_GHG_Data_Years_Jake_12062016.xls",
                      sheet = "Supplementary_Reservoir_GHG_Dat", skip = 1)
names(deemDat) = gsub(pattern = c("\\(| |#|)|/|-|\\+"), replacement = ".", 
                      x = names(deemDat))

# add 'Type'  and 'Source' columns
deemDat <- mutate(deemDat, Type = "reservoir",  # only reservoirs in Bridget's data
                 Source = "Deemer et al. 2016")

# Remove empty columns (none present here). If a column is empty in one df,
# but populated in another, read_excel can assign them different structures (i.e.
# character vs numeric), which breaks the full_join call.  Best to simply remove
# empty columns and let full_join reconcile the differences.
deemDat <- deemDat[, !apply(is.na(deemDat), 2, all)]

# Merge all lit datasets
# Full join retains all data.
litDat <- Reduce(full_join, list(bastDat, rintDat, deemDat)) 
names(litDat) <- tolower(names(litDat))

litDat <- 
  rename(litDat, # change units in subsequent 'mutate' call
         Lake_Name = system, 
         ch4.trate.mg.h_Estimate = mg.ch4.c.m.2.d.1.diffusive...ebullitive,
         ch4.drate.mg.m2.h_Estimate = mg.ch4.c.m.2.d.1.diffusive.only, 
         ch4.erate.mg.h_Estimate = mg.ch4.c.m.2.d.1.ebullitive.only,
         chla_Estimate = chlorophyll.a..ug.l.,
         mean.depth.m.morpho = mean.depth..m.,
         reservoir.area.m2 = surface.area..km2.,
         watershed.area.m2 = catchment.area..km2.,
         tn_Estimate = tn.mg.l.) %>%
  # fix units
    mutate(ch4.trate.mg.h_Estimate = ch4.trate.mg.h_Estimate/24,
           ch4.drate.mg.h_Estimate = ch4.drate.mg.m2.h_Estimate/24,
           ch4.erate.mg.h_Estimate = ch4.erate.mg.h_Estimate/24,
           reservoir.area.m2 = reservoir.area.m2 * 1000000,
           watershed.area.m2 = watershed.area.m2 * 1000000) %>%
    select(Lake_Name, source, type, ch4.trate.mg.h_Estimate, ch4.drate.mg.m2.h_Estimate,
           ch4.erate.mg.h_Estimate, chla_Estimate, mean.depth.m.morpho,
           reservoir.area.m2, watershed.area.m2, tn_Estimate)

# Bring in 2016 multi reservoir survey data
allDat <- Reduce(full_join, list(litDat, meanVariance.c.lake.lu)) 

# Bastviken style analysis---------------
ggplot(allDat,
              aes(reservoir.area.m2, (ch4.trate.mg.h_Estimate * reservoir.area.m2))) +
  geom_point(aes(color = type)) +
  scale_x_log10() +
  scale_y_log10()

# Multiple regression model 1
Mm1 <- lm(log(ch4.trate.mg.h_Estimate + 1) ~ log(reservoir.area.m2) + 
                log(chla_Estimate), 
              data = na.omit(Reduce(full_join, list(litDat, meanVariance.c.lake.lu))
                             [c("ch4.trate.mg.h_Estimate", 
                                       "reservoir.area.m2", 
                                       "chla_Estimate")]),
  stepAIC(direction = "both"))

