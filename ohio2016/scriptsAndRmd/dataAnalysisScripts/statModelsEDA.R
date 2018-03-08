#### Will Barnett, Feb 2018
#### Some exploratory data analysis with gbm package


## Packages
install.packages("gbm")
library(gbm); library(plyr); library(ggplot2)


## Load data
load('ohio2016/output/meanVariance.c.lake.lu.RData')


## A bit of data processing
## Change really long name
d <- meanVariance.c.lake.lu
with(d, table(Lake_Name))
## Acton Lake exists multiple times. Take one for the EDA.
actonInds <- grepl("Acton", d$Lake_Name)
dat <- rbind(d[!actonInds,],
             d[which(actonInds[1]),])


## Use all the data to get an idea of which variables matter.
respList <- c("ebMlHrM2_Estimate",
              "ch4.trate.mg.h_Estimate",
              "ch4.drate.mg.m2.h_Estimate",
              "ch4.erate.mg.h_Estimate",
              "co2.trate.mg.h_Estimate",
              "co2.drate.mg.m2.h_Estimate",
              "co2.erate.mg.h_Estimate")
covarList <- c("chla_Estimate", "tp_Estimate", "tn_Estimate", "max.depth.ft",
             "mean.depth.m.morpho", "prop.less.3m", "hypoxic.frac", "hypol.frac",
             "res.perimeter.m", "res.fetch.m", "reservoir.area.m2", "watershed.area.m2", 
             "percent.agg.ag", "rda", "si")
datGbm <- dat[,c(respList[2],covarList)]
## Seed
set.seed <- 4321
gbmAll = gbm(ch4.trate.mg.h_Estimate ~ .,
             distribution = "gaussian",
             data = datGbm,
             n.trees = 5000,
             shrinkage = .001, 
             n.minobsinnode = 1,
             cv.folds = 10)
nTrees <- gbm.perf(gbmAll)
par(mar=c(5,10,4,2)) # Increase y-axis margin for next plot
summary(gbmAll, las = 2) # Do these make sense to you guys?


## How well does it fit?
gbmPreds <- predict(gbmAll, newdata = datGbm, n.trees = nTrees)
predDf <- data.frame("Preds" = predict(gbmAll, newdata = datGbm, n.trees = nTrees),
                     "Data" = datGbm$ch4.trate.mg.h_Estimate)
bds <- c(0,25)
p <- ggplot(predDf, aes(x = Data, y = Preds)) + geom_point() +
  geom_abline(slope = 1, intercept = 0) + xlim(bds) + ylim(bds)
p


## This is a work-in-progress
## Define training and testing data
subProp <- .1
testRows <- sample(1:nrow(dat),
                  floor(nrow(dat)*subProp),
                  replace = FALSE)
trainRows <- which(!(1:nrow(dat) %in% testRows))
trainDat <- datGbm[trainRows,]
testDat <- datGbm[testRows,]
gbmTrain <- gbm(ch4.trate.mg.h_Estimate ~ .,
                distribution = "gaussian",
                data = trainDat,
                n.trees = nTrees,
                shrinkage = .001, 
                n.minobsinnode = 1)

