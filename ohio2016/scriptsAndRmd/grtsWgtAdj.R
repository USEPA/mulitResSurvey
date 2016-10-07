# ADJUST WEIGHTS
# Need to define 4 inputs
# 1. sitesAdj:  TRUE/FALSE corresponding to "sampled" "notsampled"
# 2. wgtAdj: original weights
# 3. wgtCat: stratum if equal area (stratified or unstratified), mdcaty if unequal
# 4. framesizeAdj: named vector containing area of:
#       -if unstratified, then whole lake
#       -if stratified-equal, then each strata
#       -if stratified-unequal, then each section

myWgtList <- list() # empty list to catch adjusted weights

for (i in 1:length(unique(eqAreaData$Lake_Name))) {
  lake.i <- unique(eqAreaData$Lake_Name)[i]
  data.i <- filter(eqAreaData, Lake_Name == lake.i)
  
# 1. sitesAdj:  TRUE/FALSE corresponding to "sampled" "notsampled"  
  sites.adj.i <- ifelse(data.i$EvalStatus == "sampled",
                        TRUE, FALSE)
  
# 2. wgtAdj: original weights  
  wgtAdj.i <- data.i$wgt

# 3. wgtCat: stratum if equal area (stratified or unstratified), section if unequal   
  if(length(unique(data.i$mdcaty)) == 1) { # one mdcaty means equal area
    wgtCat.i <- data.i$stratum
  } else {  # >1 mdcaty means unequal area
    wgtCat.i <- data.i$section
  }
  
# 4. framesizeAdj: named vector containing area of:
  if(length(unique(data.i$stratum)) == 1) { # unstratified, therefore equal in this project
    framesizeAdj.i <- distinct(data.i, Area_km2) %>% select(Area_km2)
    framesizeAdj.i <- framesizeAdj.i[ , "Area_km2"]  
    attributes(framesizeAdj.i) <- NULL
    names(framesizeAdj.i) <- c("None")
  }
  
  if(length(unique(data.i$stratum)) > 1 & length(unique(data.i$mdcaty) == 1)) { # stratified, equal
    
    framesizeAdj.ow <- filter(data.i, stratum == "open_water") %>%
      distinct(Area_km2) %>% select(Area_km2)
    
    framesizeAdj.trib <- filter(data.i, stratum == "trib") %>%
      distinct(Area_km2) %>% select(Area_km2)
    
    framesizeAdj.i <- c(framesizeAdj.ow[1,1], framesizeAdj.trib[1,1])
    attributes(framesizeAdj.i) <- NULL
    names(framesizeAdj.i) <- c("open_water", "trib")
  }
  
  if(length(unique(data.i$stratum)) > 1 & length(unique(data.i$mdcaty)) > 1) { # stratified, unequal
    nSection.i <- length(unique(data.i$section))
    section.i <- unique(data.i$section)
    
    framesizeList <- list()
    for (j in 1:nSection.i) {
      framesizeList[[j]] <- filter(data.i, section == section.i[j]) %>%
        distinct(Area_km2) %>% select(Area_km2)
    }
    
    framesizeAdj.i <- do.call("rbind", framesizeList)
    
    framesizeAdj.i <-  framesizeAdj.i[ , "Area_km2"]
    attributes(framesizeAdj.i) <- NULL
    names(framesizeAdj.i) <- section.i
  }
  
# 5. Adjust weights
  myWgtList[[i]] <- data.frame(
    Lake_Name = data.i$Lake_Name,
    siteID = data.i$siteID,
    adjWgt <- adjwgt(sites.adj.i, wgtAdj.i, wgtCat.i, framesizeAdj.i)
  )
}






# ADJUST WEIGHTS FOR STRATIFIED, EQUAL AREA
cowanSitesAdj <- ifelse(cowanData@data$EvalStatus == "sampled",
                        TRUE, FALSE)
cowanWgtAdj <- cowanData@data$wgt
cowanWgtCat <- cowanData@data$stratum  # stratum for equal area

owFramesizeAdj <- filter(cowanData@data, stratum == "open_water") %>%
  distinct(Area_km2) %>% select(Area_km2)

tribFramesizeAdj <- filter(cowanData@data, stratum == "trib") %>%
  distinct(Area_km2) %>% select(Area_km2)


cowanFramesizeAdj <- c(owFramesizeAdj[1,1], tribFramesizeAdj[1,1])
attributes(cowanFramesizeAdj) <- NULL
names(cowanFramesizeAdj) <- c("open_water", "trib")

cowanData@data$adjWgt <- adjwgt(cowanSitesAdj, cowanWgtAdj, cowanWgtCat, cowanFramesizeAdj)




# ADJUST WEIGHTS FOR STRATIFIED, UNEQUAL AREA
caesarCreekSitesAdj <- ifelse(caesarCreekData@data$EvalStatus == "sampled",
                              TRUE, FALSE)
caesarCreekWgtAdj <- caesarCreekData@data$wgt
caesarCreekWgtCat <- caesarCreekData@data$mdcaty  # mdcaty for unequal probability

# Need to define framesize by mdcaty (section) if unequal probability was used.
owFramesizeAdj1 <- filter(caesarCreekData@data, section == "north") %>%
  distinct(Area_km2) %>% select(Area_km2)

owFramesizeAdj2 <- filter(caesarCreekData@data, section == "south") %>%
  distinct(Area_km2) %>% select(Area_km2)

tribFramesizeAdj <- filter(caesarCreekData@data, section == "Equal") %>% # section == "Equal"
  distinct(Area_km2) %>% select(Area_km2)



caesarCreekFramesizeAdj <- c(owFramesizeAdj1[1,1], owFramesizeAdj2[1,1],
                             tribFramesizeAdj[1,1])
attributes(caesarCreekFramesizeAdj) <- NULL
names(caesarCreekFramesizeAdj) <- c("north", "south", "Equal")

caesarCreekData@data$adjWgt <- adjwgt(caesarCreekSitesAdj, caesarCreekWgtAdj, caesarCreekWgtCat, caesarCreekFramesizeAdj)
