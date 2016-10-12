


# EMISSION RATES AND K600 CALCULATIONS--------------------
# STEP 1:  CALCULATE EMISSION RATE VIA LINEAR AND NONLINEAR REGRESSION
#          FOR SITES WHERE PERIODS OF LINEAR ACCUMULATION ARE INDICATED 
#          IN "times.emission".
# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OF NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# STEP 3: CALCULATE K FOR CO2 AND CH4
# STEP 4: COMPARE CO2 AND CH4.  IF THE CH4 K LOOKS UNREASONABLE,
#         CALCULATE CH4 DIFFUSION USING CO2 K.
# STEP 5: IF CH4 DIFFUSION CAN'T BE CALCULATED DIRECTLY (i.e., CH4.start and CH4.stop
#         are NA), THEN CALCULATE FROM CO2 K
# STEP 6: IF NEITHER CH4 DIFFUSION NOR CO2 K CAN BE CALCULATED DIRECTLY, CALCULATE
#         CH4 DIFFUSION FROM CH4 K MEASURED AT A NEARBY STATION 
# STEP 7: CALCULATE TOTAL EMISSION RATE (diffusion + ebulition) as [CH4]final - [CH4]initial
# STEP 8: CACLULATE EBULLITION RATE AS "TOTAL - DIFFUSIVE"


# STEP 1: LINEAR AND NONLINEAR REGRESSION
n <- length(unique(paste(gga$Lake_Name, gga$siteID)))
temp <- rep(NA, n)

# Dataframe to hold results
OUT <- data.frame(site = temp, Lake_Name = temp,
                  ch4.lm.slope = temp, ch4.lm.drate.mg.h = temp, ch4.lm.aic = temp, ch4.lm.r2 = temp, ch4.lm.pval = temp,
                  ch4.ex.aic = temp, ch4.ex.r2 = temp, ch4.ex.slope = temp, ch4.ex.drate.mg.h = temp, ch4.ex.k=temp, 
                  co2.lm.slope = temp, co2.lm.drate.mg.h = temp, co2.lm.aic = temp, co2.lm.r2 = temp, co2.lm.pval = temp,
                  co2.ex.aic = temp, co2.ex.r2 = temp, co2.ex.slope = temp, co2.ex.k = temp, co2.ex.drate.mg.h = temp)


# Remove data not recorded during deployment
gga.model <- filter(gga, !is.na(Lake_Name))
pdf("ohio2016/output/figures/curveFits.pdf")
start.time <- Sys.time()
for (i in 1:length(unique(paste(gga.model$Lake_Name, gga.model$siteID)))) {  # For each unique site
  site.lake.i <- unique(paste(gga.model$siteID, gga.model$Lake_Name))[i]
  site.i <- gsub(" .*$", "", site.lake.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
  lake.i <- substr(site.lake.i, start = nchar(site.i) + 2, stop = nchar(site.lake.i)) # extract lake name
  OUT[i,"site"] <- site.i
  OUT[i,"Lake_Name"] <- lake.i  
  # Need chamber volume from eqAreaData.
  chmVol.L.i <- filter(eqAreaData, siteID == site.i, Lake_Name == lake.i) %>% 
    select(chmVol.L)   
  


  data.i.ch4 <- filter(gga.model,  # extract data
                       RDateTime >= ch4DeplyDtTm, # based on diff start time
                       RDateTime <= ch4RetDtTm, # based on diff end time
                       siteID == site.i,  
                       Lake_Name == lake.i)  %>% 
    # Calculate elapsed time (seconds).  lm behaves strangely when used with POSIXct data.
    mutate(elapTime = RDateTime - RDateTime[1], # Calculate elapsed time (seconds). 
           chmVol.L = chmVol.L.i[1,1]) %>%  # subscripting needed to remove name
    select(Lake_Name, siteID, CH4._ppm, elapTime, GasT_C, chmVol.L)  # Pull out data of interest
  
  data.i.co2 <- filter(gga.model,  # extract data
                       RDateTime >= co2DeplyDtTm, # based on diff start time
                       RDateTime <= co2RetDtTm, # based on diff end time
                       siteID == site.i,  
                       Lake_Name == lake.i)  %>% 
    # Calculate elapsed time (seconds).  lm behaves strangely when used with POSIXct data.
    mutate(elapTime = RDateTime - RDateTime[1], # Calculate elapsed time (seconds). 
           chmVol.L = chmVol.L.i[1,1]) %>%  # subscripting needed to remove name
    select(Lake_Name, siteID, CO2._ppm, elapTime, GasT_C, chmVol.L)  # Pull out data of interest
  
  # Are there data available to run the model?
  co2.indicator <- length(data.i.co2$CO2._ppm) == 0
  ch4.indicator <- length(data.i.ch4$CH4._ppm) == 0
  
  # Data needed for emission rate calcs.  Same #'s for CO2 and CH4.  Arbitrarily pulled from CO2.  
  temp.i <- if (co2.indicator) mean(data.i.ch4$GasT_C, na.rm = TRUE) else (mean(data.i.co2$GasT_C, na.rm = TRUE))  # GGA measured temp
  volume.i <- if (co2.indicator) unique(data.i.ch4[!is.na(data.i.ch4$chmVol.L), "chmVol.L"]) else
    unique(data.i.co2[!is.na(data.i.co2$chmVol.L), "chmVol.L"])# Dome volume
  
  # lm
  lm.ch4.i <- try(lm(data.i.ch4$CH4._ppm ~ data.i.ch4$elapTime), silent = TRUE)  # suppress warning if fails 
  lm.co2.i <- try(lm(data.i.co2$CO2._ppm ~ data.i.co2$elapTime), silent = TRUE)  # linear regression
  
  # lm slopes
  slope.ch4.i <- if(ch4.indicator) NA else (as.numeric(coef(lm.ch4.i)[2]))  # lm slope: ppm s-1   
  slope.co2.i <- if(co2.indicator) NA else (as.numeric(coef(lm.co2.i)[2]))   # lm slope: ppm s-1
  OUT[i, c("ch4.lm.slope", "co2.lm.slope")] <- c(slope.ch4.i, slope.co2.i)
  
  # lm p-values
  fstat.ch4 <- if(ch4.indicator) rep(NA,3) else summary(lm.ch4.i)$fstatistic
  fstat.co2 <- if(co2.indicator) rep(NA,3) else summary(lm.co2.i)$fstatistic
  OUT[i, c("ch4.lm.pval")]  <- pf(fstat.ch4[1], fstat.ch4[2], fstat.ch4[3], lower.tail = FALSE)
  OUT[i, c("co2.lm.pval")]  <- pf(fstat.co2[1], fstat.co2[2], fstat.co2[3], lower.tail = FALSE)
  
  # lm r2 values
  OUT[i, c("ch4.lm.r2")]  <- if(ch4.indicator) NA else summary(lm.ch4.i)["r.squared"]
  OUT[i, c("co2.lm.r2")]  <- if(co2.indicator) NA else summary(lm.co2.i)["r.squared"]
  
  # lm AIC values
  OUT[i, c("ch4.lm.aic")] <- if(ch4.indicator) NA else AIC(lm.ch4.i)
  OUT[i, c("co2.lm.aic")] <- if(co2.indicator) NA else AIC(lm.co2.i)
  
  # Exponential Model
  cmax.ch4 <- data.i.ch4$CH4._ppm[max(which(!is.na(data.i.ch4$CH4._ppm)))]  # cmax = final CH4
  c.initial.ch4 <- data.i.ch4$CH4._ppm[min(which(!is.na(data.i.ch4$CH4._ppm)))]  # initial CH4 
  exp.ch4.i <-try(nlsLM(CH4._ppm~cmax-(cmax-b)*exp(-k*as.numeric(elapTime)),
                        data = data.i.ch4, start=list(cmax=cmax.ch4, b=cmax.ch4-c.initial.ch4, k=.03)),
                  silent = TRUE) 
  
  cmax.co2 <- data.i.co2$CO2._ppm[max(which(!is.na(data.i.co2$CO2._ppm)))]  # cmax = final CO2
  c.initial.co2 <- data.i.co2$CO2._ppm[min(which(!is.na(data.i.co2$CO2._ppm)))]  # initial CO2   
  exp.co2.i <-try(nlsLM(CO2._ppm~cmax-(cmax-b)*exp(-k*as.numeric(elapTime)),
                        data = data.i.co2, start=list(cmax=505, b= 400, k=0.004)),
                  silent=TRUE) 
  # Ex r2
  rss.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else sum(residuals(exp.ch4.i)^2)
  tss.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else 
    sum((data.i.ch4$CH4._ppm - mean(data.i.ch4$CH4._ppm, na.rm=TRUE))^2, na.rm=TRUE)
  OUT[i, "ch4.ex.r2"] = 1 - rss.ch4.i/tss.ch4.i
  
  rss.co2.i <- if(class(exp.co2.i) == "try-error") NA else sum(residuals(exp.co2.i)^2)
  tss.co2.i <- if(class(exp.co2.i) == "try-error") NA else 
    sum((data.i.co2$CO2._ppm - mean(data.i.co2$CO2._ppm, na.rm=TRUE))^2, na.rm=TRUE)
  OUT[i, "co2.ex.r2"] = 1 - rss.co2.i/tss.co2.i
  
  # Ex AIC
  OUT[i, "ch4.ex.aic"] = if(class(exp.ch4.i) == "try-error") NA else AIC(exp.ch4.i)
  OUT[i, "co2.ex.aic"] = if(class(exp.co2.i) == "try-error") NA else AIC(exp.co2.i)
  
  # Ex slope
  coef.exp.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else coef(exp.ch4.i)  
  OUT[i, "ch4.ex.slope"] = if(class(exp.ch4.i) == "try-error") NA else 
    coef.exp.ch4.i["k"]*(coef.exp.ch4.i["cmax"]-coef.exp.ch4.i["b"])  # ppm s-1
  
  coef.exp.co2.i <- if(class(exp.co2.i) == "try-error") NA else coef(exp.co2.i)  
  OUT[i, "co2.ex.slope"] = if(class(exp.co2.i) == "try-error") NA else 
    coef.exp.co2.i["k"]*(coef.exp.co2.i["cmax"]-coef.exp.co2.i["b"])  # ppm s-1
  
  #Ex k  
  OUT[i, "ch4.ex.k"] = if(class(exp.ch4.i) == "try-error") NA else 
    coef.exp.ch4.i["k"]
  OUT[i, "co2.ex.k"] = if(class(exp.co2.i) == "try-error") NA else 
    coef.exp.co2.i["k"]
  
  # Emission rate.  Assumes atmospheric pressure of 1 atm.
  # Converting from parts per million to umole cross out.  No conversion factor necessary. Dome area = 0.2 m2
  ch4.lm.drate.i.umol.s <- ((volume.i * 1 * slope.ch4.i) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CH4 s-1
  OUT[i, "ch4.lm.drate.mg.h"] = if (length(ch4.lm.drate.i.umol.s) == 0)  # throws error if no data
    NA else
      ch4.lm.drate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.lm.drate.i.umol.s <- ((volume.i * 1 * slope.co2.i) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  OUT[i, "co2.lm.drate.mg.h"] =  if (length(co2.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      co2.lm.drate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  ch4.ex.drate.i.umol.s <- ((volume.i * 1 * OUT[i, "ch4.ex.slope"]) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CH4 s-1
  OUT[i, "ch4.ex.drate.mg.h"] = if (length(ch4.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      ch4.ex.drate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.ex.drate.i.umol.s <- ((volume.i * 1 * OUT[i, "co2.ex.slope"]) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  OUT[i, "co2.ex.drate.mg.h"] =  if (length(co2.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      co2.ex.drate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  # Plots
  # CH4 first
  ch4.ex.pred <- try(  
    data.frame(ch4.pred = predict(exp.ch4.i, newdata = data.i.ch4), # pred values from exponential model
               elapTime = data.i.ch4$elapTime),
    silent = TRUE)
  
  ch4.title <- paste(OUT[i, "site"], # plot title
                     OUT[i, "Lake_Name"],
                     "ex.r2=",
                     round(OUT[i, "ch4.ex.r2"], 2),
                     "ex.AIC=",
                     round(OUT[i, "ch4.ex.aic"],2),
                     "ex.rate=",
                     round(OUT[i, "ch4.ex.drate.mg.h"], 2),                    
                     "\n lm.r2=",
                     round(OUT[i, "ch4.lm.r2"],2),
                     "lm.AIC=",
                     round(OUT[i, "ch4.lm.aic"],2),
                     "lm.rate=",
                     round(OUT[i, "ch4.lm.drate.mg.h"], 2),
                     sep=" ")
  p.ch4 <- ggplot(data.i.ch4, aes(as.numeric(elapTime), CH4._ppm)) + 
    geom_point() +
    xlab("Seconds") +
    ggtitle(ch4.title) +
    stat_smooth(method = "lm", se=FALSE)
  if (class(exp.ch4.i) == "try-error") p.ch4 else  # if exp model worked, add exp line
    p.ch4 <- p.ch4 + geom_line(data=ch4.ex.pred, aes(as.numeric(elapTime), ch4.pred), color = "red")
  print(p.ch4)
  
  
  # CO2 models
  co2.ex.pred <- try(
    data.frame(co2.pred = predict(exp.co2.i, newdata = data.i.co2),  # pred data from exp model
               elapTime = data.i.co2$elapTime),
    silent=TRUE)
  
  co2.title <- paste(OUT[i, "site"], # plot title
                     OUT[i, "Lake_Name"],
                     "ex.r2=",
                     round(OUT[i, "co2.ex.r2"], 2),
                     "ex.AIC=",
                     round(OUT[i, "co2.ex.aic"],2),
                     "ex.rate=",
                     round(OUT[i, "co2.ex.drate.mg.h"], 2),                    
                     "\n lm.r2=",
                     round(OUT[i, "co2.lm.r2"],2),
                     "lm.AIC=",
                     round(OUT[i, "co2.lm.aic"],2),
                     "lm.rate=",
                     round(OUT[i, "co2.lm.drate.mg.h"], 2),
                     sep=" ")
  p.co2 <- ggplot(data.i.co2, aes(as.numeric(elapTime), CO2._ppm)) + 
    geom_point() +
    xlab("Seconds") +
    ggtitle(co2.title) +
    stat_smooth(method = "lm", se=FALSE)
  if (class(exp.co2.i) == "try-error") p.co2 else  # if exp model worked, add exp line
    p.co2 <- p.co2 + geom_line(data=co2.ex.pred, aes(as.numeric(elapTime), co2.pred), color = "red")
  print(p.co2)
}  
dev.off()
start.time;Sys.time() 

# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OR NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# Choose best rate.  Just use AIC
OUT <- mutate(OUT, co2.drate.mg.h.best = ifelse(co2.lm.aic < co2.ex.aic, co2.lm.drate.mg.h, co2.ex.drate.mg.h),
              ch4.drate.mg.h.best = ifelse(ch4.lm.aic < ch4.ex.aic, ch4.lm.drate.mg.h, ch4.ex.drate.mg.h)) 
# Inspect r2.  Remember, this includes "b" sites and dups
plot(with(OUT,ifelse(co2.lm.aic < co2.ex.aic, co2.lm.r2, co2.ex.r2)))  # CO2: only 1 with r2 < 0.9
plot(with(OUT,ifelse(ch4.lm.aic < ch4.ex.aic, ch4.lm.r2, ch4.ex.r2)))  # CH4:  Three just below 0.9, 1 below 0.8
filter(data.frame(r2 = with(OUT,ifelse(ch4.lm.aic < ch4.ex.aic, ch4.lm.r2, ch4.ex.r2)),
                  site = OUT$site),
       r2 < 0.9)  # site 108 is 0.895, which rounds up to 0.9
# site 40failed is no concern
# site 49 (r2=0.80) and site 18 (r2=0.894) should be addressed.

# STEP 3: CALCULATE K FOR CO2 AND CH4
# Pull in dissolved gas and calculate k600
OUT.dGas <- merge(OUT, dGas, all=TRUE)  # This also brings in dGas from free flowing sites
OUT.dGas <- mutate(OUT.dGas, kco2.cm.h = (co2.drate.mg.h.best/CO2.seacarb.ex.mg.m3)*100,
                   kch4.cm.h = (ch4.drate.mg.h.best/CH4.ex.mg.m3)*100)

# STEP 4: COMPARE CO2 AND CH4 k.  IDENTIFY SITES WHERE THE CH4 K LOOKS UNREASONABLE.
#         CALCULATE CH4 DIFFUSION USING CH4-k from nearby sites.  
#         Can't use CO2 K due to possible CED.
ggplot(OUT.dGas, aes(kco2.cm.h, kch4.cm.h, label=site)) + 
  geom_point() + 
  geom_abline(intercept=0, slope=1) +
  geom_text()

ggplot(OUT.dGas, aes(CH4.ex.mg.m3, ch4.drate.mg.h.best, label=site)) + 
  geom_point() + 
  geom_abline(intercept=0, slope=1) +
  geom_text()

# All outlier kCH4 values also have outlier CH4-diff values.  17 ebulition dominated, 124 looks OK but k is too high, 
# 89 is ebullition dominated, 85 is ebulition dominated, 92 looks nice, but probably ebulition dominated

# STEP 5: IF CH4 DIFFUSION CAN'T BE CALCULATED DIRECTLY (i.e., CH4.start and CH4.stop
#         are NA, OR CALCULATED kCH4 IS TOO HIGH, SEE ABOVE), THEN CALCULATE USING CO2 K MEASURED AT SAME SITE IF pH < 8.75.
#         THIS CRITERION IS BASED ON BADE ET AL 2006 (FIG.2).  ELSEWISE, CALCULATE ch4-diff USING K CH4 FROM A NEARBY SITE.

# Step 5.1. Calculate k.ch4.cm.h from CO2 emission data when ch4-diff calc is bad or kCH4 is too high (i.e.
#           5 sites identified in STEP 4 above). This is only done when the CO2 emission data is good and pH<8.5.
#           Use estimated k.ch4 (i.e. kco2.to.ch4) to calculate ch4-diff.

# Need to merge OUT.dGas and sonde data to filter based on is.na(ch4-diff) and pH.
# The object "seaInput" contains the sonde data, alkalinity data, and AES data.
# See "readAlkalinityandRelatedData.R"
OUT.dGas.sonde <- merge(OUT.dGas, seaInput)
bad.dch4.sites.low.ph <- rbind(  # the code below produces two dataframes.  rbind them together
  filter(OUT.dGas.sonde, is.na(ch4.drate.mg.h.best) & #  ch4-diff is bad
           pH < 8.5 &   # pH is low
           !is.na(co2.drate.mg.h.best)) %>% # CO2-diff is good
    select(site),  # pull out site
  filter(OUT.dGas.sonde, site %in% c(17,124,89,85,92) &  # five sites with high kCH4 from above
           pH < 8.5 &  # pH is low
           !is.na(co2.drate.mg.h.best)) %>%  # CO2-diff is good
    select(site)  # pull out site
) #  14 sites had ch4-diff calculated from kco2 measured at same site.  Only 6 from final 115 site survey.
# Calculate kco2-->kch4 and ch4-diff
OUT.dGas.sonde <- mutate(OUT.dGas.sonde, 
                         Sc.ch4 = 1897.8-(114.28*Temp.C)+(3.2902*(Temp.C^2))-(0.039061*(Temp.C^3)),  # calculate Schmidt for CH4
                         Sc.co2 = 1911.1-(118.11*Temp.C)+(3.4527*(Temp.C^2))-(0.04132*(Temp.C^3)),  # calculate Schmidt for CO2
                         kco2.to.ch4 = kco2.cm.h*((Sc.ch4/Sc.co2)^-0.5),  #  CO2 to CH4 conversion factor
                         ch4.drate.mg.h.best2 = ifelse(site %in% bad.dch4.sites.low.ph$site, # if kCH4 is to high, diffusion wasn't calculated, and pH<8.5
                                                       (kco2.to.ch4*CH4.ex.mg.m3)/100,  # CH4 emission rate from CH4 excess and kCO2-->kCH4
                                                       ch4.drate.mg.h.best))  # Else, don't change

# Step 5.2  Estimate ch4-diff for the cases where the original ch4-diff calc was bad, kch4 was too high (i.e.
#           5 sites identified in STEP 4 above) AND pH > 8.5 or dCO2 was bad.  ch4-diff estimate will be made
#           using kch4 measured at nearby site.
# Sites that meet criteria
bad.dch4.sites.high.ph <- 
  rbind(
    filter(OUT.dGas.sonde, is.na(ch4.drate.mg.h.best) & #  ch4-diff is bad
             (pH > 8.5 |   # pH is high
                is.na(co2.drate.mg.h.best)))  %>%  # dCO2 is bad
      select(site),  # pull out site
    filter(OUT.dGas.sonde, site %in% c(17,124,89,85,92) &  # five sites with high kCH4 from above
             (pH > 8.5 |  # pH is low
                is.na(co2.drate.mg.h.best))) %>%  # dCO2 is bad
      select(site)  # pull out site
  )  # A total of 27, but only 23 of 115 in final design.

# kch4.cm.h.est calculation
{
  OUT.dGas.sonde[OUT.dGas.sonde$site == 1, "kch4.cm.h.est"] =  
  mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(33), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 101, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(133), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 109, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(125,114), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 115, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(93), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 121, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(105), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == "143b", "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(107), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 2, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(93), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 22, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(10,38), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 26, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(42,54), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 29, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(41,25), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 32, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(44,48,16), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 34, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(58,50), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 37, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(21), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 5, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(118), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 50, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(14,58), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 57, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(25,53), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 59, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(31,11,15,43), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 61, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(25,53,45), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == "81b", "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(108, 110,"94b", 94, 106), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 82, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(118, 86, 102), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 83, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(99), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 9, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(25,53), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 98, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(125,114), "kch4.cm.h"])
  OUT.dGas.sonde[OUT.dGas.sonde$site == 17, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(33,49), "kch4.cm.h"]) # calculated kch4 too high, pH>8.5
  OUT.dGas.sonde[OUT.dGas.sonde$site == 85, "kch4.cm.h.est"] =  
    mean(OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(133), "kch4.cm.h"],
         OUT.dGas.sonde[OUT.dGas.sonde$site %in% c(89), "kco2.to.ch4"])  # calculated kch4 too high, pH>8.5
  }

# Use kch4.cm.h.est to calculate ch4.diff for sites where ch4-diff couldn't be calculated 
# any other way
OUT.dGas.sonde <- mutate(OUT.dGas.sonde,
                         ch4.drate.mg.h.best2 = ifelse(is.na(kch4.cm.h.est), 
                                                       ch4.drate.mg.h.best2,
                                                       (kch4.cm.h.est*CH4.ex.mg.m3)/100),
                         kch4.cm.h.final = ifelse(site %in% bad.dch4.sites.low.ph$site,
                                                  kco2.to.ch4,
                                                  ifelse(site %in% bad.dch4.sites.high.ph$site,
                                                         kch4.cm.h.est,
                                                         kch4.cm.h))
)

ggplot(OUT.dGas.sonde, aes(ch4.drate.mg.h.best2, kch4.cm.h.final, label=site)) + #  K and emission all look reasonable
  geom_point() + 
  geom_abline(intercept=0, slope=1) +
  geom_text()


# STEP 6: CALCULATE TOTAL EMISSION RATE
# CALCULATE TOTAL EMISSION RATE AS FINAL MEASUREMENT - INITIAL MEASUREMENT
n <- length(unique(sonde.gga.track$site))  # will calculate for each site
temp <- rep(NA, n)

# Dataframe to hold results
TOT <- data.frame(site = temp, ch4.delta = temp, elapTime = temp, ch4.trate.mg.h = temp)

for (i in 1:length(unique(sonde.gga.track$site))) {  # For each unique site
  site.i <- unique(sonde.gga.track$site)[i]  # Identify unique site
  TOT[i,"site"] <- site.i  # Enter site into dataframe
  data.i <- sonde.gga.track[sonde.gga.track$site == site.i & !is.na(sonde.gga.track$CH4._ppm), ]  # Extract data, omit NAs
  
  # Are there data available to run the model?
  ch4.indicator <- length(data.i$CH4._ppm) == 0
  
  # Data needed for emission rate calcs.  
  temp.i <- if (ch4.indicator) NA else (mean(data.i$GasT_C, na.rm = TRUE))  # GGA measured temp
  volume.i <- if (ch4.indicator) NA else unique(data.i[!is.na(data.i$volume.L), "volume.L"])  # dome volume
  TOT[i, "ch4.delta"] <- data.i[length(data.i$CH4._ppm), "CH4._ppm"] - data.i[1, "CH4._ppm"]  # delta [CH4]
  TOT[i, "elapTime"] <- difftime(data.i[length(data.i$CH4._ppm), "RDateTime"], data.i[1, "RDateTime"], units="secs")  # delta time in sec
  slope.trate.ch4.ppm.s <- TOT[i, "ch4.delta"] / TOT[i, "elapTime"]  # rate, ppm/s
  # Emission rate.  Assumes atmospheric pressure of 1 atm.
  # Converting from parts per million to umole cross out.  No conversion factor necessary. Dome area = 0.164 m2  
  ch4.trate.umol.m2.s <- ((volume.i * 1 * slope.trate.ch4.ppm.s) / (0.082057 * (temp.i + 273.15))) / 0.164 #umol CH4 s-1
  # if no evidence of ebullition in visual inspection of data, then set equal to diffusion rate
  TOT[i, "ch4.trate.mg.h"] = ifelse(times.emission[times.emission$site == site.i, "ebul"] == TRUE, 
                                    ch4.trate.umol.m2.s * (16/1000) * (60*60),  # mg CH4 m-2 h-1
                                    OUT.dGas.sonde[OUT.dGas.sonde$site == site.i, "ch4.drate.mg.h.best2"])
}

# Merge diffusive and total emission data.
OUT.TOT.dGas.sonde <- merge(TOT[, c("site", "ch4.trate.mg.h")], OUT.dGas.sonde, all=TRUE)



# STEP 7.  CALCULATE EBULITION RATE
# Bring in times.emission data to indicate when ebullition was observed
OUT.TOT.dGas.sonde.times <- merge(OUT.TOT.dGas.sonde, times.emission[,c("site", "ebul")], all=TRUE)
OUT.TOT.dGas.sonde.times$ch4.erate.mg.h <- with(OUT.TOT.dGas.sonde.times, ch4.trate.mg.h - ch4.drate.mg.h.best2)

# A few quick plots.  Looking good!!
# ordered factor for Cleveland dotplot
OUT.TOT.dGas.sonde.times$fsite <- factor(OUT.TOT.dGas.sonde.times$site, 
                                         levels=OUT.TOT.dGas.sonde.times[order(OUT.TOT.dGas.sonde.times$ch4.trate.mg.h), "site"])
ggplot(OUT.TOT.dGas.sonde.times, aes(ch4.trate.mg.h, fsite)) + geom_point()  # total
ggplot(OUT.TOT.dGas.sonde.times, aes(ch4.drate.mg.h.best2, fsite)) + geom_point()  # diffusion
ggplot(OUT.TOT.dGas.sonde.times, aes(ch4.erate.mg.h, fsite)) + geom_point()  # ebullition

# STEP 8.  MINOR CLEANING 
OUT.TOT.dGas.sonde.times <- select(OUT.TOT.dGas.sonde.times, -ch4.drate.mg.h.best) %>%
  rename(ch4.drate.mg.h = ch4.drate.mg.h.best2)
