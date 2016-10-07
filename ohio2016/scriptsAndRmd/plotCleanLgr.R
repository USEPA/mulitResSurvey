# THIS SCRIPT WILL BE USED TO PLOT AND CLEAN LGR DATA IN PREPARATION
# FOR CALCULATION OF DIFFUSIVE EMISSION RATE

#1. INSPECT INSTANCES OF NA IN GGA
# Time/date stamp first
filter(gga, is.na(RDateTime))
# there are a bunch of NA for this field, likely related to corrupt LGR files.  Will just strip out for now.
gga <- filter(gga, !is.na(RDateTime))  # strip out missing RDateTime.  They complicate functions below.


#2.  LOOP TO ASSIGN LAKE NAME, SITEID, AND DEPLY TIME TO LGR OBSERVATIONS.
# Many rows in eqAreaData have NA for chmDeplyDtTm.  For example, at all oversample
# sites where chambers were not deployed.  We want to remove these rows, or the missing
# values complicate the loop.

missingChmDeplyDtTm <- is.na(eqAreaData$chmDeplyDtTm) # logical for missing chamber deployment times

for (i in 1:length(eqAreaData[!missingChmDeplyDtTm, "chmDeplyDtTm"])) {  # exclude rows with no deployment time
  chmDeplyDtTm.i <- eqAreaData[!missingChmDeplyDtTm, "chmDeplyDtTm"][i] # exclude rows with no deployment time, select ith observation
  Lake_Name.i <- eqAreaData[!missingChmDeplyDtTm, "Lake_Name"][i] # exclude rows with no deployment time, select ith observation
  siteID.i <- eqAreaData[!missingChmDeplyDtTm, "siteID"][i]  # exclude rows with no deployment time, select ith observation

  # Create logical indicator to indicate time window corresponding to
  # the ith lake and siteID.  Add 1 minute to deployment and retrieval time to expand
  # x-axis range during plotting.
  logicalIndicator.i <- gga$RDateTime > (chmDeplyDtTm.i - 60) & # 1 min < field notes
    gga$RDateTime < (chmDeplyDtTm.i + (6*60))# 6 min > field notes.  Retr time not recorded, assume 5 min after deployment

  gga[logicalIndicator.i, "Lake_Name"] = Lake_Name.i # Set Lake_Name
  gga[logicalIndicator.i, "siteID"] = siteID.i # Set siteID 
  gga[logicalIndicator.i, "co2DeplyDtTm"] = chmDeplyDtTm.i # Set chamber deployment time 
  gga[logicalIndicator.i, "co2RetDtTm"] = chmDeplyDtTm.i + (60*5) # Set chamber deployment time 
  gga[logicalIndicator.i, "ch4DeplyDtTm"] = chmDeplyDtTm.i # Set chamber deployment time 
  gga[logicalIndicator.i, "ch4RetDtTm"] = chmDeplyDtTm.i + (60*5) # Set chamber deployment time   
}

# POSIXct class was stripped during the loop.  Put it back in here.
gga[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] <- 
lapply(gga[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")], 
       as.POSIXct, origin = "1970-01-01 00:00:00", tz= "UTC")  # set tz!

#3. RECORD ADJUSTMENTS TO TIME SERIES PLOTS
# Lake_Name, siteID, co2DeplyDtTm, co2RetDtTm, ch4DeplyDtTm, ch4RetDtTm
# This order is critical!
adjData <- c("Alum Creek Lake", "SU-02", "2016-06-07 15:10:05", "2016-06-07 15:17:00", "2016-06-07 15:10:05", "2016-06-07 15:17:00",
             "Alum Creek Lake", "SU-01", "2016-06-07 15:39:40", "2016-06-07 15:44:55", "2016-06-07 15:39:40", "2016-06-07 15:44:55",
             "Alum Creek Lake", "SU-05", "2016-06-07 16:01:40", "2016-06-07 16:04:00", "2016-06-07 16:01:40", "2016-06-07 16:04:00",
             "Alum Creek Lake", "SU-04", "2016-06-07 16:51:40", "2016-06-07 16:55:00", "2016-06-07 16:51:40", "2016-06-07 16:55:00",
             "Alum Creek Lake", "SU-06", "2016-06-07 17:08:30", "2016-06-07 17:12:00", "2016-06-07 17:08:30", "2016-06-07 17:12:00",
             "Alum Creek Lake", "SU-07", "2016-06-07 17:48:00", "2016-06-07 17:52:00", "2016-06-07 17:48:00", "2016-06-07 17:52:00",
             "Alum Creek Lake", "SU-34", "2016-06-07 14:05:30", "2016-06-07 14:09:00", "2016-06-07 14:05:30", "2016-06-07 14:09:00",
             "Alum Creek Lake", "SU-31", "2016-06-07 14:20:00", "2016-06-07 14:23:00", "2016-06-07 14:20:00", "2016-06-07 14:23:00",
             "Alum Creek Lake", "SU-35", "2016-06-07 14:32:05", "2016-06-07 14:35:00", "2016-06-07 14:32:05", "2016-06-07 14:35:00",
             "Alum Creek Lake", "SU-32", "2016-06-07 14:45:10", "2016-06-07 14:49:00", "2016-06-07 14:45:10", "2016-06-07 14:49:00",
             "Alum Creek Lake", "SU-29", "2016-06-07 15:00:00", "2016-06-07 15:03:00", "2016-06-07 15:00:00", "2016-06-07 15:03:00",
             "Delaware Reservoir", "S-30", "2016-06-08 13:41:00", "2016-06-08 13:46:00", "2016-06-08 13:40:30", "2016-06-08 13:44:00",
             "Delaware Reservoir", "S-32", "2016-06-08 14:06:30", "2016-06-08 14:08:00", "2016-06-08 14:06:30", "2016-06-08 14:08:00",   #after ebullition
             "Delaware Reservoir", "S-35", "2016-06-08 14:37:00", "2016-06-08 14:40:00", "2016-06-08 14:37:30", "2016-06-08 14:40:00",  #after ebullition
             "Delaware Reservoir", "S-33", "2016-06-08 15:04:00", "2016-06-08 15:08:00", "2016-06-08 15:04:00", "2016-06-08 15:08:00",
             "Delaware Reservoir", "S-29", "2016-06-08 15:19:00", "2016-06-08 15:22:00", "2016-06-08 15:19:00", "2016-06-08 15:22:00",
             "Delaware Reservoir", "S-34", "2016-06-08 15:30:00", "2016-06-08 15:34:00", "2016-06-08 15:30:00", "2016-06-08 15:34:00",
             "Delaware Reservoir", "S-05", "2016-06-08 15:42:15", "2016-06-08 15:46:00", "2016-06-08 15:42:15", "2016-06-08 15:46:00",
             "Delaware Reservoir", "S-29", "2016-06-08 15:54:00", "2016-06-08 15:56:00", "2016-06-08 15:53:00", "2016-06-08 15:53:50",
             "Delaware Reservoir", "S-07", "2016-06-08 16:13:00", "2016-06-08 16:17:00", "2016-06-08 16:13:00", "2016-06-08 16:17:00",
             "Delaware Reservoir", "S-03", "2016-06-08 16:37:00", "2016-06-08 16:41:00", "2016-06-08 16:37:00", "2016-06-08 16:41:00",
             "Delaware Reservoir", "S-06", "2016-06-08 16:52:00", "2016-06-08 16:55:00", "2016-06-08 16:52:00", "2016-06-08 16:55:00",
             "Delaware Reservoir", "S-04", "2016-06-08 17:22:00", "2016-06-08 17:25:00", "2016-06-08 17:22:00", "2016-06-08 17:25:00",
             "Delaware Reservoir", "S-08", "2016-06-08 17:46:00", "2016-06-08 17:50:00", "2016-06-08 17:41:00", "2016-06-08 17:46:00", #very high CH4, recheck
             "Knox Lake", "S-37", "2016-06-09 15:17:00", "2016-06-09 15:20:00", "2016-06-09 15:17:00", "2016-06-09 15:20:00",
             "Knox Lake", "S-31", "2016-06-09 15:40:00", "2016-06-09 15:43:00", "2016-06-09 15:40:00", "2016-06-09 15:43:00",
             "Knox Lake", "S-35", "2016-06-09 15:54:00", "2016-06-09 15:57:00", "2016-06-09 15:54:00", "2016-06-09 15:57:00",
             "Knox Lake", "S-32", "2016-06-09 16:02:00", "2016-06-09 16:06:00", "2016-06-09 16:02:00", "2016-06-09 16:02:40", #ebullition
             "Knox Lake", "S-07", "2016-06-09 16:20:00", "2016-06-09 16:24:00", "2016-06-09 16:20:00", "2016-06-09 16:24:00",
             "Knox Lake", "S-03", "2016-06-09 16:41:00", "2016-06-09 16:45:00", "2016-06-09 16:41:00", "2016-06-09 16:45:00",
             "Knox Lake", "S-03", "2016-06-09 16:41:00", "2016-06-09 16:45:00", "2016-06-09 16:41:00", "2016-06-09 16:45:00",
             "Knox Lake", "S-06", "2016-06-09 16:51:00", "2016-06-09 16:55:00", "2016-06-09 16:51:00", "2016-06-09 16:55:00",
             "Knox Lake", "S-10", "2016-06-09 17:01:00", "2016-06-09 17:03:00", "2016-06-09 17:01:00", "2016-06-09 17:03:00",
             "Knox Lake", "S-02", "2016-06-09 17:16:00", "2016-06-09 17:19:00", "2016-06-09 17:18:00", "2016-06-09 17:19:00", #ebullition
             "Knox Lake", "S-04", "2016-06-09 17:28:00", "2016-06-09 17:32:00", "2016-06-09 17:28:00", "2016-06-09 17:29:00", #ebullition
             "Knox Lake", "S-08", "2016-06-09 17:42:00", "2016-06-09 17:44:00", "2016-06-09 17:42:00", "2016-06-09 17:44:00", #ebullition
             "Knox Lake", "S-09", "2016-06-09 18:10:10", "2016-06-09 18:15:00", "2016-06-09 18:10:10", "2016-06-09 18:15:00",
             "Knox Lake", "S-05", "2016-06-09 18:22:00", "2016-06-09 18:27:00", "2016-06-09 18:22:00", "2016-06-09 18:24:30",
             "Knox Lake", "S-01", "2016-06-09 18:50:30", "2016-06-09 18:53:00", "2016-06-09 18:50:30", "2016-06-09 18:53:00", #eb
             "Kiser Lake", "U-12", "2016-06-13 12:40:00", "2016-06-13 12:45:00", "2016-06-13 12:38:00", "2016-06-13 12:44:00", #high CH4 -- 17 ppm
             "Kiser Lake", "U-12", "2016-06-13 12:40:00", "2016-06-13 12:45:00", "2016-06-13 12:38:00", "2016-06-13 12:44:00",
             
             
             
             )

# Coerce to data.frame  
adjDataDf <- data.frame(Lake_Name = adjData[seq(1,length(adjData), 6)],
                             siteID = adjData[seq(2,length(adjData), 6)],
                             co2DeplyDtTm = adjData[seq(3,length(adjData), 6)],
                             co2RetDtTm = adjData[seq(4,length(adjData), 6)],
                             ch4DeplyDtTm = adjData[seq(5,length(adjData), 6)],
                             ch4RetDtTm = adjData[seq(6,length(adjData), 6)],
                        stringsAsFactors = FALSE)

# Convert date/time data from character to POSIXct
adjDataDf[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] <- 
  lapply(adjDataDf[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")], 
    as.POSIXct, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")  # set tz!

#4. UPDATE DEPLOYMENT AND RETRIEVAL TIMES BASED ON FIXES ABOVE (SEE POINT 3)
  
  for (i in 1:with(adjDataDf, length(unique(paste(siteID, Lake_Name))))) { # for each unique site x lake combination
    lake.i <- adjDataDf$Lake_Name[i]  # extract ith lake
    site.i <- adjDataDf$siteID[i]  # extract ith site
    data.i <- adjDataDf[i, ]  # extract data.i
    
    #Calculate earliest and latest observation we need for this lake x site.  Simply min/max deply time.
    #This will make sure x-axis range is good for time series plots.
    #Use do.call the concentate "c" the df (which is actually a list) to a vector that
    #can be fed to min, while preserving the POSIXct attribute.
    #http://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
    #Unfortunately, this changes tz, which must be manually reset to UTC
    start.time.i <- min(do.call("c", data.i[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]))
    end.time.i <- max(do.call("c", data.i[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]))
    attr(start.time.i, "tzone") <- "UTC"  # reset time zone!
    attr(end.time.i, "tzone") <- "UTC"  # reset time zone!
    
    #Logical indicator indicator block of gga data that should be updated
    #The extra minute and begining and end extend x-axis range for plotting,
    #which is good for picking time range for modeling diffusion.
    logicalIndicator.i <- gga$RDateTime > (start.time.i - 60) & # 1 min < deployment
      gga$RDateTime < (end.time.i + 60) # 1 min > retrieval
    
    # Replace original time stamps with updated numbers
    # POSIXct and time zone preserved through this step.  Wow!
    gga[logicalIndicator.i, c("Lake_Name", "siteID", "co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] =
      data.i[, c("Lake_Name", "siteID", "co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]
}




#5.  PLOT CO2 AND CH4 PROFILES FOR INSPECTION

pdf("ohio2016/output/figures/ggaProfile.pdf", paper = "a4r") # landscape orientation

for (i in 1:with(gga[!is.na(gga$Lake_Name), ], # this eliminates observations without a Lake_Name (LGR data when chamber not deployed)
                 length(unique(paste(siteID, Lake_Name))))) {  # each combination of site and lake
  
  site.lake.i <- with(gga[!is.na(gga$Lake_Name), ],  # extract unique lake x site combination
                      unique(paste(siteID, Lake_Name)))[i]
  site.i <- gsub(" .*$", "", site.lake.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
  lake.i <- substr(site.lake.i, start = nchar(site.i) + 2, stop = nchar(site.lake.i)) # extract lake name
  data.i <- filter(gga, Lake_Name == lake.i, siteID == site.i) %>%  # Pull out GGA data chunk
    select(-GasT_C) # No need to plot gas temperature
  RDate.i <- unique(data.i$RDate)  # for panel title

  plot.i <- ggplot(data.i,  aes(x = RDateTime, y = CH4._ppm)) + 
          geom_point() +
          geom_vline(data = data.i, aes(xintercept = as.numeric(ch4DeplyDtTm))) +
          geom_vline(data = data.i, aes(xintercept = as.numeric(ch4RetDtTm))) +
          scale_x_datetime(labels=date_format("%H:%M")) +
          ggtitle(paste(lake.i, site.i, RDate.i)) +
          theme(axis.text.x = element_text(size = 7),
                plot.title = element_text(size = 11))
  
  plot.ii <- ggplot(data.i,  aes(x = RDateTime, y = CO2._ppm)) + 
          geom_point() +
          geom_vline(data = data.i, aes(xintercept = as.numeric(co2DeplyDtTm))) +
          geom_vline(data = data.i, aes(xintercept = as.numeric(co2RetDtTm))) +
          scale_x_datetime(labels=date_format("%H:%M")) +
          ggtitle(paste(lake.i, site.i)) +
          theme(axis.text.x = element_text(size = 7))
  
  grid.arrange(plot.i, plot.ii, ncol = 2) # use to put two plots per page
}
dev.off()  #5 min, 50 seconds










