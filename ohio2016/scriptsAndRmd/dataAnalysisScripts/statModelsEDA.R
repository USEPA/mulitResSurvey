#### Will Barnett, Feb 2018
#### Some exploratory data analysis with gbm package


## Packages
# install.packages("gbm", "ggplot2")
# library(ggplot2, lib.loc = "~/Rlibs/"); library(sp, lib.loc = "~/Rlibs/"); library(raster, lib.loc = "~/Rlibs/"); library(dismo, lib.loc = "~/Rlibs/")


## Load data
load('ohio2016/output/meanVariance.c.lake.lu.RData')


## A bit of data processing
## Change really long name
d <- meanVariance.c.lake.lu
with(d, table(Lake_Name))

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


## Acton Lake exists multiple times. Aggregate the entries in a 
## reasonable way.
actonInds <- grepl("Acton", d$Lake_Name)
dat <- d[actonInds,c(respList, gsub("Estimate","StdError",respList),
                     covarList)]
outVec <- NULL
for(i in 1:ncol(dat)){
  # i = 8
  # Estimates can be averaged.
  tmpNm <- names(dat)[i]
  if(!grepl("StdError", tmpNm)){
    aggEst <- mean(dat[,i], na.rm = TRUE)
  }else {
    # Count the number of non-NA SE estimates
    numNA <- sum(is.na(dat[,i]))
    # The variance of independent random variables is just the sum of the variances.
    # Since we're taking a mean of random variables here, we're really
    # interested in the variance of the mean of those variables. This is just
    # the sum of the variances * (1 / n)^2
    wt <- 1/(nrow(dat) - numNA)
    aggEst <- sqrt(wt^2 * sum(dat[,i]^2))
  }
  outVec <- c(outVec, aggEst)
}
tmp <- data.frame(t(outVec))
names(tmp) <- names(dat)
datGbm <- rbind(tmp, d[!actonInds,c(respList, gsub("Estimate","StdError",respList),
                                   covarList)])

## Function for investigating best fitting GBM's.
evalGBM <- function(x, resp, covar, weights=NULL, nTrees = 10000,
                    shrMin=0.005, shrMax = 10e-6, bfMin = 0.5,
                    bfMax = 0.9, cvFolds = 10, trainProp = 0.9, n = 10){
  ## x is a data frame with a response variable, and covariates
  ## weights is a vector of numbers
  ## nTrees is the number of trees
  ## shrMin and shrMax are upper and lower bounds for shrinkage in gbm()
  ## bfMin and bfMax are upper and lower bounds for bag.fraction in gbm()
  ## trainProp is the proportion of the data to use for training (vs. testing)
  ## cvFolds is passed on to gbm() as cv.folds argument
  ## n is he number of items in a sequence for shrinkage and bag.fraction
  
  ## Example 
  # x <- datGbm[,c("ch4.trate.mg.h_Estimate",covarList)]
  # resp = "ch4.trate.mg.h_Estimate"; covar = covarList; weights = 1/datGbm$ch4.trate.mg.h_StdError^2; nTrees = 10000
  # shrMin=0.005; shrMax = 10e-6; bfMin = 0.5; bfMax = 0.9; cvFolds = 10; trainProp = 0.75; n = 10
  gbmFormula <- as.formula(paste(resp,"~",paste(covar, collapse="+")))
  parmGrid <- expand.grid(seq(shrMin, shrMax, length.out = n),
                          seq(bfMin, bfMax, length.out = n))
  names(parmGrid) <- c("shr", "bf")
  parmGrid$isMSE <- as.numeric(NA)
  parmGrid$osMSE <- as.numeric(NA)
  parmGrid$optTrees <- as.integer(NA)
  if(is.null(weights)){
    wts <- rep(1, nrow(x))
  }else {
   wts <- weights 
  }
  
  ## Loop
  for(i in 1:nrow(parmGrid)){
    # i = 1
    isMSE <- NULL
    osMSE <- NULL
    numTrees <- NULL
    for(j in 1:10){
      # i = 1; j = 1
      trainInds <- sample(1:nrow(x), floor(nrow(x)*trainProp))
      tmpTrain <- x[trainInds,]
      tmpTest <- x[-trainInds,]
      tmpGbm <- gbm(formula = gbmFormula, 
                    distribution = "gaussian",
                    data = tmpTrain,
                    weights = wts[trainInds],
                    n.trees = nTrees,
                    n.minobsinnode = 2,
                    interaction.depth = 2,
                    shrinkage = parmGrid$shr[i],
                    bag.fraction = parmGrid$bf[i],
                    cv.folds = cvFolds)
      optTrees <- gbm.perf(tmpGbm)
      
      ## MSE
      isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = optTrees)
      mse_is <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
      isMSE <- c(isMSE,mse_is)
      osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = optTrees)
      mse_os <- sum(c(isPreds - tmpTest[,resp])^2)/nrow(tmpTest)
      osMSE <- c(osMSE,mse_os)
      numTrees <- c(numTrees, optTrees)
    }
    ## Find average MSE over 10 folds
    parmGrid$isMSE[i] <- mean(isMSE)
    parmGrid$osMSE[i] <- mean(osMSE)
    parmGrid$optTrees[i] <- mean(numTrees)
  }
  
  ## Make a contour plot for the  shrinkage and bag.fraction surface
  p1 <- ggplot(parmGrid, aes(x = shr, y =  bf, z = osMSE)) +
    xlab("Shrinkage Rate") + ylab("Bag Fraction") + ggtitle("Out-of-sample MSE") +
    geom_raster(aes(fill = osMSE)) +
    geom_contour(colour = "white")
  p2 <- ggplot(parmGrid, aes(x = shr, y =  bf, z = isMSE)) +
    xlab("Shrinkage Rate") + ylab("Bag Fraction") + ggtitle("In-sample MSE") +
    geom_raster(aes(fill = isMSE)) +
    geom_contour(colour = "white")
  return(list("parameterGrid"=parmGrid, "plots"=list(p1,p2)))
}


## Testing the function
x <- datGbm[,c("ch4.trate.mg.h_Estimate",covarList)]
resp = "ch4.trate.mg.h_Estimate"
covar = covarList 
weights = 1/datGbm$ch4.trate.mg.h_StdError^2 
nTrees = 10000
evalch4trate <- evalGBM(x <- datGbm[,c("ch4.trate.mg.h_Estimate",covarList)],
        resp = "ch4.trate.mg.h_Estimate",
        covar = covarList,
        weights = 1/datGbm$ch4.trate.mg.h_StdError^2,
        nTrees = 10000)
# Look at plots
evalch4trate$plots[[1]]
evalch4trate$plots[[2]]
evalch4trate$parameterGrid
