library(readxl)  # For reading Excel files
library(gdata)   # Also for reading Excel files
library(ggplot2) # For plotting
library(gridExtra) # For plotting
library(scales)  # For plotting
# library(persp3d) # For plotting response surface in 3d; Not compatable w/R 3.3.0
library(reshape) # For merge_recurse function
library(reshape2) # For melt/dcast
library(plyr)  # for 'join' in ggplot plotting of shapefile
library(dplyr)   # For data manipulation
library(knitr)   # To knit rmarkdown document
library(ggmap)   # For ggmap plot of reservoirs
library(rgdal)   # For reading shapefiles
library(spsurvey)  # survey design
library(maptools) # for ggplot plotting of shapefile (fortify function)
library(minpack.lm) # for non linear diffusion model


# Functions
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


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
  data.cont <- data.frame(siteID=x$siteID,
                          ebMlHrM2=x$ebMlHrM2, # volume of gas in trap
                          chla=x$chla_S)
  
  
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

