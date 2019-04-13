# MISC STATS FOR MANUSCRIPT

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


# Diffusive emission rate model fits
co2.ex.r2
co2.lm.r2
ch4.ex.r2
ch4.lm.r2

