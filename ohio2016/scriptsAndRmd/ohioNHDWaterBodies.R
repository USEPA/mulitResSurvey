library(rgdal)
library(dplyr)
library(ggplot2)

# Read data
ohioWB <- readOGR(dsn = "inputData/spatial", layer = "NHDWaterbody")
str(ohioWB@data)

# FCodes codes for reservoirs from http://nhd.usgs.gov/userguide.html
reservoir.Fcode <- c(43600, 43603, 43613:43621) # non treatement reservoirs
reservoir.Fcode.trt <- c(43601, 43604, 43605, 43606, 43607, 43609, 43611, 43612, 43623:43626) #trt ponds
ohioWB@data <- mutate(ohioWB@data,
                      reservoir = ifelse(FCode %in% reservoir.Fcode,
                               TRUE,
                               FALSE))

sum(ohioWB@data$reservoir) # wow, 410 reservoirs
ggplot(filter(ohioWB@data, reservoir == TRUE), aes(AreaSqKm)) + geom_histogram()

