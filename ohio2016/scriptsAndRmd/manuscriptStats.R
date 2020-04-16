# MISC STATS FOR MANUSCRIPT

# METHODS-----------------------------------------------
# Trap deployment and retrieval time
deplyTime <- strftime(eqAreaData$trapDeplyDtTm, # strip data, to character
                      tz = "UTC", # needed to maintain original tz
                      format = "%H:%M:%S") %>%
  as.POSIXct(format = "%H:%M:%S", tz = "UTC") # back to time object, but all with same date
summary(deplyTime)

retrTime <- strftime(eqAreaData$trapRtrvDtTm, #strip data, to character
                     tz = "UTC",
                     format = "%H:%M:%S") %>%
  as.POSIXct(format = "%H:%M:%S") # back to time object, but all with same date
summary(retrTime) # 06:12 am retrieval?

# Which lake has a 06:12 retrieval time?
filter(eqAreaData, trapRtrvDtTm == as.POSIXct("2016-08-31 06:12:00", #Milton
                                              tz = "UTC"))
# This looks like typo, should probably 9:12, fix later
filter(eqAreaData, Lake_Name == "Lake Milton") %>%
  select(trapRtrvDtTm, siteID)

# for now, filter 06:12, then summarize
retrTime <- strftime(
  eqAreaData[eqAreaData$trapRtrvDtTm != as.POSIXct("2016-08-31 06:12:00", #Milton
                                                   tz = "UTC"), "trapRtrvDtTm"], #strip data, to character
             tz = "UTC",
             format = "%H:%M:%S") %>%
  as.POSIXct(format = "%H:%M:%S") # back to time object, but all with same date
summary(retrTime) # 07:33 am is earliest


eqAreaData %>% mutate(trpDur = trapRtrvDtTm - trapDeplyDtTm) %>%
  select(trpDur) %>% 
  summarize(medDur = median(trpDur, na.rm = TRUE), 
            minDur = min(trpDur, na.rm = TRUE), 
            maxDur = max(trpDur, na.rm = TRUE))



#  Table 1. Covariates
write.table(allCovar, "ohio2016/manuscript/table1/allCovar.txt")
write.table(nationalCovar, "ohio2016/manuscript/table1/nationalCovar.txt")
table1 <- localDataGbm %>% select(allCovar) %>% 
  # https://rdrr.io/cran/dplyr/man/summarise_all.html
  summarise_all(list(~mean(.), ~min(.), ~max(.))) %>% # descriptive stats
  melt() %>%
  # create new column for statistic type
  mutate(statistic = ifelse(grepl(pattern = "_max", x = variable),
                            "max",
                            ifelse(grepl(pattern = "_mean", x = variable),
                                   "mean",
                                   ifelse(grepl(pattern = "_min", x = variable),
                                          "min",
                                          NA))),
         variable = gsub(pattern = c("_mean|_max|_min"), # clean variable names
                         replacement = "", 
                         x = variable)) %>%
  dcast(variable ~ statistic) # cast to wide
write.table(table1, "ohio2016/manuscript/table1/table1.txt")


# Diffusive emission rate model fits
co2.ex.r2
co2.lm.r2
ch4.ex.r2
ch4.lm.r2

# RESULTS-----------
# Morphology / chemistry
{
summary(localDataGbm$reservoir.area.m2 / 1000000)
summary(localDataGbm$max.depth.m)
summary(localDataGbm$percent.agg.ag)
summary(localDataGbm$watershed.area.m2 / 1000000)
summary(localDataGbm$tp_Estimate)
summary(localDataGbm$tn_Estimate)
summary(localDataGbm$chla_Estimate)
cor(localDataGbm[, c("tp_Estimate", "tn_Estimate", 
                     "chla_Estimate", "percent.agg.ag")])

filter(localDataGbm, hypol.frac > 0) %>% # 24 stratified
  summarise(n.obs = n())

filter(localDataGbm, hypoxic.frac > 0) %>% # 22 hypoxic
  summarise(n.obs = n())
}

# CH4 emissions
{
# Dissolved
summary(localDataGbm$dissolved.ch4_Estimate * 1000000) # CH4 umol/l
summary(localDataGbm$ch4.sat.ratio_Estimate) # CH4 saturation

# Diffusive CH4 emissions
summary(eqAreaData$ch4.drate.mg.h.best) # all diffusive CH4 measurements

filter(eqAreaData, !is.na(ch4.drate.mg.h.best)) %>% # 491 diffusive CH4 measurements
  select(ch4.drate.mg.h.best) %>% summarise(n=n())

summary(localDataGbm$ch4.drate.mg.m2.h_Estimate) # reservoir scale CH4 emissions

# Ebullitive emissions
summary(eqAreaData$ch4.erate.mg.h) # ebullitive CH4 measurements

filter(eqAreaData, !is.na(ch4.erate.mg.h)) %>% # 536 ebullitive CH4 measurements
  select(ch4.erate.mg.h) %>% summarise(n=n())

summary(localDataGbm$ch4.erate.mg.h_Estimate) # reservoir scale ebullitive CH4


# Total CH4 emissions
# Any differences in total CH4 emission rate among strata.
tCh4ByStrata <- filter(meanVariance.c) %>% # cast to wide
  filter(Subpopulation != "lake") %>%
  select(Lake_Name, Subpopulation, 
         ch4.trate.mg.h_LCB95Pct, ch4.trate.mg.h_UCB95Pct,
         ch4.trate.mg.h_Estimate) %>%
  melt() %>%
  cast(Lake_Name ~ Subpopulation + variable)

tribCh4trate <- matrix(c(tCh4ByStrata$trib_ch4.trate.mg.h_LCB95Pct, # put 95% CI for trib tRate into matrix
                         tCh4ByStrata$trib_ch4.trate.mg.h_UCB95Pct), 
                       ncol = 2)
owCh4trate <- matrix(c(tCh4ByStrata$open_water_ch4.trate.mg.h_LCB95Pct, # put 95% CI for ow tRate into matrix
                       tCh4ByStrata$open_water_ch4.trate.mg.h_UCB95Pct), 
                     ncol = 2)  

# Apply overlaps function to each row or matrices
overlap. <- vector() # empty vector for results
for (i in 1:nrow(tribCh4trate)) {
  overlap.[i] <- tribCh4trate[i, ] %overlaps% owCh4trate[i, ] # TRUE if 95% CI overlap
}

table(overlap.) #17 overlap, 10 do not

# trib > OW in 8 of 10 without overlapping 95% confidence intervals
tCh4ByStrata[!overlap., c("trib_ch4.trate.mg.h_Estimate", "open_water_ch4.trate.mg.h_Estimate")]

# trib vs strata, not paying attention to 95% confidence intervals
tCh4ByStrata %>%
  mutate(tribGreaterOW <- ifelse(trib_ch4.trate.mg.h_Estimate >
                                   open_water_ch4.trate.mg.h_Estimate,
                                 "trib", "ow")) %>%
  {table(.$tribGreaterOW)} # 21 trib rates exceed OW rates

tCh4ByStrata %>%
  mutate(tribDividedOW = trib_ch4.trate.mg.h_Estimate/
                                   open_water_ch4.trate.mg.h_Estimate) %>%
  summary(tribDividedOW)

summary(eqAreaData$ch4.trate.mg.h) # 536 total CH4 measurements. Why > diffCh4?  Because diffusion failed at Cowan and other sites had p>0.05 curve fits.
  

summary(localDataGbm$ch4.trate.mg.h_Estimate)
summary(localDataGbm$ch4.erate.mg.h_Estimate / # 
          localDataGbm$ch4.trate.mg.h_Estimate) * 100
}


# CO2 results
{
# saturation status
localDataGbm %>% # 20 super saturated, 12 undersaturated
  mutate(co2sat = ifelse(co2.sat.ratio_Estimate > 1,
                         "sat",
                         "undersat")) %>%
  select(co2sat) %>%
  {table(.$co2sat)}

summary(localDataGbm$dissolved.co2_Estimate * 1000000)
summary(localDataGbm$co2.sat.ratio_Estimate)


# Diffusive emissions
summary(localDataGbm$co2.drate.mg.m2.h_Estimate)
localDataGbm %>% # 20 super saturated, 12 undersaturated
  mutate(co2srcsnk = ifelse(co2.drate.mg.m2.h_Estimate > 0,
                         "src",
                         "snk")) %>%
  select(co2srcsnk) %>%
  {table(.$co2srcsnk)}

# total emissions
summary(localDataGbm$co2.trate.mg.h_Estimate) # lake scale

# Use this to look for differences in total CO2 among strata
# Do 95% confidence intervals overlap?
# trib > ow in 5 lakes; ow > trib in 1 lake.  Overlap in all other lakes.
tCo2ByStrata <- meanVariance.c %>% 
  filter(Subpopulation != "lake") %>%
  select(Lake_Name, Subpopulation, 
         co2.trate.mg.h_LCB95Pct, co2.trate.mg.h_UCB95Pct,
         co2.trate.mg.h_Estimate) %>%
  melt() %>%
  cast(Lake_Name ~ Subpopulation + variable)

tribCo2trate <- matrix(c(tCo2ByStrata$trib_co2.trate.mg.h_LCB95Pct, # put 95% CI for trib tRate into matrix
                         tCo2ByStrata$trib_co2.trate.mg.h_UCB95Pct), 
                       ncol = 2)
owCo2trate <- matrix(c(tCo2ByStrata$open_water_co2.trate.mg.h_LCB95Pct, # put 95% CI for ow tRate into matrix
                       tCo2ByStrata$open_water_co2.trate.mg.h_UCB95Pct), 
                     ncol = 2)  

# Apply overlaps function to each row or matrices
overlap.co2 <- vector() # empty vector for results
for (i in 1:nrow(tribCo2trate)) {
  overlap.co2[i] <- tribCo2trate[i, ] %overlaps% owCo2trate[i, ] # TRUE if 95% CI overlap
}

table(overlap.co2) #18 overlap, 8 do not

# trib > OW in 5 of 8 without overlap
tCo2ByStrata[!overlap., c("trib_co2.trate.mg.h_Estimate", "open_water_co2.trate.mg.h_Estimate")]



# Ebullition relative to diffusion
summary(localDataGbm$co2.erate.mg.h_Estimate/ 
          abs(localDataGbm$co2.drate.mg.m2.h_Estimate)) * 100


# Total CO2 emissions
localDataGbm %>% # 21 sinks, 11 sources
  mutate(co2SrcSnk = ifelse(co2.trate.mg.h_Estimate > 0,
                            "src",
                            "snk")) %>%
  select(co2SrcSnk) %>%
  {table(.$co2SrcSnk)}

}

# Bubble composition----------
summary(eqAreaData$trap_ch4.ppm / 10000)

eqAreaData %>%  # 336
  select(trap_ch4.ppm) %>%
  filter(!is.na(trap_ch4.ppm)) %>%
  summarize(nBubble = length(trap_ch4.ppm))

summary(eqAreaData$trap_co2.ppm / 10000)

# CO2 EQUIVALENTS------------------
source("ohio2016/scriptsAndRmd/co2Equiv.R")
gwp <- select(meanVariance.c.lake.lu.agg, 
              ch4.co2.eq.mg.h_Estimate, co2.trate.mg.h_Estimate, tot.Co2eq.mg.h_Estimate) %>%
  mutate(src.snk = ifelse(tot.Co2eq.mg.h_Estimate < 0,
                          "sink",
                          "source"),
         ch4proportion = ch4.co2.eq.mg.h_Estimate / tot.Co2eq.mg.h_Estimate)

summary(gwp$totCo2eq)
table(gwp$src.snk) # 11 sink / 21 source

filter(gwp, 
       tot.Co2eq.mg.h_Estimate > 0,  # 10 CO2 sinks, but GWP source
       co2.trate.mg.h_Estimate < 0) %>%
  summarise(co2.sink.but.positive.gwp = n())




# DISCUSSION--------------
# Bevelhimer
filter(natDataGbm, citation == "Bevelhimer") %>%
  select(Lake_Name, ch4.trate.mg.h_Estimate)

summary(localDataGbm$ch4.trate.mg.h_Estimate) # 0.5 - 25
select(localDataGbm, Lake_Name, ch4.trate.mg.h_Estimate) %>%
  arrange(ch4.trate.mg.h_Estimate)










# Ebullition x depth

ggplot(eqAreaData, aes(wtrDpth, ch4.erate.mg.h)) + geom_point()


# Variance
# compare 95% CI to mean.
with(localDataGbm, 
     summary((ch4.trate.mg.h_UCB95Pct - ch4.trate.mg.h_LCB95Pct) / ch4.trate.mg.h_Estimate))


with(localDataGbm, 
     summary((co2.trate.mg.h_UCB95Pct - co2.trate.mg.h_LCB95Pct) / abs(co2.trate.mg.h_Estimate)))


# 95% CI relative to mean
numberSites <- eqAreaData %>%
  group_by(Lake_Name) %>%
  summarise(numberSites = sum(EvalStatus == "sampled")) %>%
  filter(!grepl(pattern = c("Aug|July|Oct"), x = Lake_Name)) # exclude extra Acton Lake
confAnal <- merge(numberSites,
                  localDataGbm) %>%
  mutate(relCertCh4 = ((ch4.trate.mg.h_UCB95Pct - ch4.trate.mg.h_LCB95Pct) / ch4.trate.mg.h_Estimate) * 100,
         relCertCo2 = abs(((co2.trate.mg.h_UCB95Pct - co2.trate.mg.h_LCB95Pct) / co2.trate.mg.h_Estimate) * 100))

summary(confAnal$relCertCh4) # 72%
summary(confAnal$relCertCo2) # 58%%
filter(natDataGbm, citation == "Harrison") %>%
  mutate(density = 4 / (reservoir.area.m2/1000000)) %>%
  select(density)

# local vs SRS variance estimator

srsVariance <- meanVarianceSRS.c.lake %>%
  mutate(srsVar = ch4.trate.mg.h_UCB95Pct - ch4.trate.mg.h_LCB95Pct) %>%
  select(Lake_Name, srsVar)


localVariance <- meanVariance.c.lake %>%
  mutate(locVar = ch4.trate.mg.h_UCB95Pct - ch4.trate.mg.h_LCB95Pct) %>%
  select(Lake_Name, locVar)

merge(srsVariance, localVariance) %>%  # 1- 0.77 = 23% lower
  mutate(relVar = locVar / srsVar) %>%
summary(relVar)

# SI DATA-----------
# names of response variables, including variance estimates
respVars <- names(localDataGbm)[grepl(pattern = c("drate|trate|erate"),
                                      x = names(localDataGbm)) &
                                  !grepl(pattern = c("n2o|Std"), # exclude stdError and n2o data
                                         x = names(localDataGbm))]

write.table(x = localDataGbm %>% select(Lake_Name, nhdCOMID, respVars, allCovar), 
            file = "ohio2016/manuscript/siTable.txt",
            row.names = FALSE)

# Sample date, Lake_Name, COMID
merge(eqAreaData %>% as_tibble() %>% filter(!is.na(deplyDt)) %>% 
  select(Lake_Name, deplyDt, ) %>% distinct(Lake_Name, deplyDt) %>% 
  arrange(deplyDt),
  meanVariance.c.lake.lu.agg %>% select(Lake_Name, nhdCOMID)) %>%
  write.table("ohio2016/output/siTable1.txt", row.names = FALSE)
