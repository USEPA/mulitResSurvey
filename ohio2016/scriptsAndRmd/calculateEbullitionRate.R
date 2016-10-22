# First calculate volumetric ebullion rate.  Straightforward operation
# That can be vectorized across the entire df.
eqAreaData <- mutate(eqAreaData, 
                     ebMlHrM2 = TtTrpVl / 
                       (as.numeric(trapRtrvDtTm - trapDeplyDtTm) * 
                          ((3.14*.28^2)))) # diameter = 22.25in=0.56m, r=.28m))

# Mass flux rate must be calculated by Lake.  Tried to apply by group using
# by_group, ddply, and lapply.  I couldn't figure it out, resorted to for loop

myEbList <- list()
for (i in 1:length(unique(eqAreaData$Lake_Name))) {
  lake.i <- unique(eqAreaData$Lake_Name)[i]
  data.i <- filter(eqAreaData, Lake_Name == lake.i )
  out.ch4 <- mass.rate(data.i, choice1 = "ch4") 
  out.co2 <- mass.rate(data.i, choice1 = "co2")
  out.n2o <- mass.rate(data.i, choice1 = "n2o")
  
  myEbList[[i]] <- data.frame(ebCh4mgM2h = out.ch4,
                              ebCo2mgM2h = out.co2,
                              ebN2omgM2h = out.n2o,
                              Lake_Name = data.i$Lake_Name,
                              siteID = data.i$siteID)
}

ebResults <- do.call("rbind", myEbList)  # This coerces the list into a dataframe. Cool..

str(eqAreaData) # 1426 observations
str(ebResults)  # 1426 observations
eqAreaData <- merge(eqAreaData,ebResults, all = TRUE) 
str(eqAreaData) # 1426 observations

ggplot(eqAreaData, aes(co2.drate.mg.h.best, ebCo2mgM2h)) +
  geom_point()
