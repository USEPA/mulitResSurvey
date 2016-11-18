# LIBRARIES---------------
library(readxl)  # For reading Excel files
library(gdata)   # Also for reading Excel files
library(ggplot2) # For plotting
library(gridExtra) # For plotting
library(scales)  # For plotting
library(rgl) # For plotting interactive response surface in 3d
#library(persp3D) # Not compable with R 3.3.0
library(scatterplot3d)  # for plotting
library(reshape) # For merge_recurse function
library(reshape2) # For melt/dcast
library(tidyr)  # for separate
library(plyr)  # for 'join' in ggplot plotting of shapefile
library(dplyr)   # For data manipulation
library(knitr)   # To knit rmarkdown document
library(ggmap)   # For ggmap plot of reservoirs
library(rgdal)   # For reading shapefiles
library(spsurvey)  # survey design
library(maptools) # for ggplot plotting of shapefile (fortify function)
library(minpack.lm) # for non linear diffusion model

# car for variance inflation factor (vif)
# http://www.statmethods.net/stats/rdiagnostics.html)
library(car) # vif function
library(fmsb) # variance inflation factor 'VIF' function
library(relaimpo)

# TRIM FUNCTION--------------------------
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# GRTS ANALYSIS FUNCTION------------------
# Analyze continuous variable from grts survey design.
grtsMeanVariance <- function(x) {
  
  # FIRST, DEFINE FRAMESIZE.  DEPENDS ON WHETHER STRATIFIED OR NOT.
  if(length(unique(x$stratum)) == 1) {  # if unstratified
    framesize.tmp = select(x, Area_km2) %>% distinct(Area_km2)
    framesize <- c("lake" = sum(framesize.tmp$Area_km2)) # sum needed to accomodate multiple mdcaty
  }
  
  if(length(unique(x$stratum)) > 1) {  # if stratified
    owFramesize.tmp <- filter(x, stratum == "open_water") %>%
      select(Area_km2) %>% distinct(Area_km2)
    owFramesize <- sum(owFramesize.tmp$Area_km2) # sum needed to accomodate multiple mdcaty
    
    tribFramesize.tmp <- filter(x, stratum == "trib") %>%
      select(Area_km2) %>% distinct(Area_km2)
    tribFramesize <- sum(tribFramesize.tmp$Area_km2) # sum needed to accomodate multiple mdcaty
    
    framesize <- c("open_water" = owFramesize, "trib" = tribFramesize)
  }
  
  
  # DEFINE NUMBER OF ROWS IN DATAFRAME
  nRows <- nrow(x)
  
  
  
  # CREATE SITES DATAFRAME
  sites <- data.frame(siteID=x$siteID,
                      Use=x$EvalStatus == "sampled")  # use sampled sites
  
  
  
  # SUBPOP DATAFRAME
  if(length(unique(x$stratum)) == 1) {  # if unstratified
    subpop <- data.frame(siteID=x$siteID,
                         lake=rep("lake", nRows))
  }
  
  if(length(unique(x$stratum)) > 1) {  # if stratified
    subpop <- data.frame(siteID=x$siteID,
                         lake=rep("lake", nRows),
                         stratum=x$stratum)
  }
  
  
  # DESIGN DATAFRAME
  design <- data.frame(siteID=x$siteID,
                       wgt=x$adjWgt,
                       xcoord=x$xcoord,
                       ycoord=x$ycoord)
  
  
  # DATA.CONF data frame.
  data.cont <- data.frame(siteID = x$siteID,
                          ebMlHrM2 = x$ebMlHrM2, # volume of gas in trap
                          chla = x$chla_S,
                          tp = x$TP,
                          tn = x$TN,
                          tnh4 = x$TNH4,
                          tno2 = x$TNO2,
                          trap_ch4.ppm = x$trap_ch4.ppm,
                          #tno2-3 = x$TNO-3, # this breaks code.  need to remove dash
                          ch4.drate.mg.m2.h = x$ch4.drate.mg.h.best,
                          co2.drate.mg.m2.h = x$co2.drate.mg.h.best,
                          ch4.erate.mg.h = x$ch4.erate.mg.h,
                          co2.erate.mg.h = x$co2.erate.mg.h,
                          n2o.erate.mg.h = x$n2o.erate.mg.h,
                          co2.trate.mg.h = x$co2.trate.mg.h,
                          ch4.trate.mg.h = x$ch4.trate.mg.h)
  
  
  # CALCULATE CDF ESTIMATES
  if(length(unique(x$stratum)) == 1) {  # if unstratified
    cdf.final <- cont.analysis(sites, subpop, design, data.cont,
                               popsize=list(lake=sum(framesize)))
  }
  
  if(length(unique(x$stratum)) > 1) {  # if stratified
    cdf.final <- cont.analysis(sites, subpop, design, data.cont,
                               popsize=list(lake=sum(framesize),
                                            stratum=as.list(framesize)))
  }
  cdf.final
}

# EBULLITION MASS FLUX FUNCTION------------------------

# Function for calculating mass flux rate--                  
mass.rate <- function(X1, choice1){
  # trap gas data to use if measured values aren't available
  trap_ch4.ppm <- ifelse(is.na(X1$trap_ch4.ppm), mean(X1$trap_ch4.ppm, na.rm=TRUE), X1$trap_ch4.ppm) 
  trap_co2.ppm <- ifelse(is.na(X1$trap_co2.ppm), mean(X1$trap_co2.ppm, na.rm=TRUE), X1$trap_co2.ppm)
  trap_n2o.ppm <- ifelse(is.na(X1$trap_n2o.ppm), mean(X1$trap_n2o.ppm, na.rm=TRUE), X1$trap_n2o.ppm)
  
  # barometric pressure needed: n=PV/RT
  bp <- ifelse(is.na(mean(X1$BrPrssr, na.rm=TRUE)),
               1,
               mean(X1$BrPrssr, na.rm=TRUE)/760)
  
  # temperature needed
  gas.temp <- ifelse(is.na(X1$Tmp_C_S),
                     273.15 + 20, # assume 20C if not measured
                     273.15 + X1$Tmp_C_S)
  
  # convert 1mL to moles
  mL.to.mmoles <- ((bp*0.001)/(0.082058 * gas.temp)) * 1000      #1mL = 0.001L; *100 to convt to mmol       
  
  # convert mmoles to mg
  if(choice1 == "ch4") {mg.gas <- mL.to.mmoles * 16 * (trap_ch4.ppm/1000000)}  #16mg/mmole
  if(choice1 == "co2") {mg.gas <- mL.to.mmoles * 44 * (trap_co2.ppm/1000000)}  #44mg/mmole
  if(choice1 == "n2o") {mg.gas <- mL.to.mmoles * 44 * (trap_n2o.ppm/1000000)}  #44mg/mmole
  
  # calculate rate
  mass.flux.rate <- mg.gas * X1$ebMlHrM2 #bubble rate in mg ch4-co2-n2o /day/m2

  # return mass flux rate in mg ch4-co2-n2o /day/m2
  mass.flux.rate
}

# ORDER Lake_Name FUNCTION-------------------------
orderLake <- function(x, choice1) {
  if(choice1 == "ch4.d") {
    column <- "ch4.drate.mg.m2.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if(choice1 == "ch4.e") {
    column <- "ch4.erate.mg.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if(choice1 == "ch4.t") {
    column <- "ch4.trate.mg.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if(choice1 == "co2.d") {
    column <- "co2.drate.mg.m2.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if(choice1 == "co2.e") {
    column <- "co2.erate.mg.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if(choice1 == "co2.t") {
    column <- "co2.trate.mg.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if (choice1 == "vol") {
    column <- "ebMlHrM2_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  }
  
}



# Custom variance inflation factor----------------------
# See blog at https://www.r-bloggers.com/collinearity-and-stepwise-vif-selection/
# Can source function from gist; but can't get to work
"https://gist.githubusercontent.com/fawda123/4717702/raw/a84b776b6145c9c8f2adf93de517eab97d42cfa9/vif_fun.r"
# Define function here
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

