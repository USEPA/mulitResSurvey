# SCRIPT FOR EVALUATING RUN TO RUN VARIABILITY WHEN USING K MEANS CLUSTERING
# TO CREATE TRAINING DATA SETS.  ALSO INVESTIGATING HOW REPRODUCABILITY IS 
# AFFECTED BY THE PROPORTION OF THE MAIN DATA THAT IS USED FOR TRAINING DATA.

# Start by writing a function to execute the simulations
# FUNCTION FOR INVESTIGATING BEST FITTING GBM's----------------------
#### Will Barnett, Feb 2018
evalGBMKmeans <- function(x, resp, covar, weights=NULL, nTrees = 20000,
                    shr = 0.0005, bf = 0.9, 
                    cvFoldsMin = 5, cvFoldsMax = 10,
                    trainPropMin = 0.5, trainPropMax = 0.9,
                    nGridCv = 6, nGridProp = 5, nGBM = 50, setSeed = 2345){
  ## x is a data frame with a response variable, and covariates
  ## weights is a vector of numbers
  ## nTrees is the number of trees
  ## cvFoldsMin and cvFoldsMax are upper and lower bounds for cv.folds in gbm()
  ## trainPropMin and trainPropMax are upper and lower bounds for trainProp in kMeansTrainSet()
  ## nGridProp is the number of items in a sequence for trainProp
  ## nGridCv is the number of items in a sequence for cvFolds
  
  ## Example 
  # x <- localDataGbm
  # resp = "ch4.trate.mg.h_Estimate"; covar = allCovar; weights = 1/localDataGbm$ch4.trate.mg.h_StdError^2; nTrees = 20000
  # shr = 0.0005; bf = 0.9; cvFoldsMin = 2; cvFoldsMax = 10; trainPropMin = 0.5
  # trainPropMax = 0.9; nGridCv = 5; nGridProp = 5; nGBM = 50; setSeed = 2345
  gbmFormula <- as.formula(paste(resp,"~",paste(covar, collapse="+")))
  parmGrid <- expand.grid(seq(cvFoldsMin, cvFoldsMax, length.out = nGridCv),
                          seq(trainPropMin, trainPropMax, length.out = nGridProp))
  names(parmGrid) <- c("cvFolds", "trainProp")
  # Repeat rown in parmGrid for each replicate simulation
  # I don't fully understand this code, but it gets the job done.
  # https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
  parmGrid <- parmGrid %>% slice(rep(1:n(), nGBM)) %>%
    arrange(trainProp, cvFolds)
  parmGrid$isMSE <- as.numeric(NA)
  parmGrid$osMSE <- as.numeric(NA)
  parmGrid$optTrees <- as.integer(NA)
  if(is.null(weights)){
    wts <- rep(1, nrow(x))
  }else {
    wts <- weights 
  }
  
  ## Loop
  set.seed(setSeed)
  for(i in 1:5){ #nrow(parmGrid)){
    message(i)
    # i = 1
    # isMSE <- NULL
    # osMSE <- NULL
    # numTrees <- NULL
    x <- x[, c(resp, covar)] # remove uneeded columns,  Must so this before kMeans
      trainInds <- kMeansTrainSet(x, k = 5, trainProp = parmGrid$trainProp[i])
      tmpTrain <- x[trainInds,]
      tmpTest <- x[-trainInds,]
      
      repeat{ # Repeat running gbm until...
      tmpGbm <- gbm(formula = gbmFormula, 
                    distribution = "gaussian",
                    data = tmpTrain,
                    weights = wts[trainInds],
                    n.trees = nTrees,
                    n.minobsinnode = 2,
                    interaction.depth = 2,
                    shrinkage = shr,
                    bag.fraction = bf,
                    cv.folds = parmGrid$cvFolds[i])
      optTrees <- gbm.perf(tmpGbm)
      if (optTrees > 1000 & optTrees < 19990){ # if good # of trees
        break # then break
      } # otherwise keep going
      }
      ## MSE
      isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = optTrees)
      parmGrid$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
      osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = optTrees)
      parmGrid$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
      parmGrid$optTrees[i] <- optTrees
  }
}

# RUN SIMULATIONS WITH TOTAL CH4 EMISSION RATE, LOCAL OBSERVATIONS, ALLCOVAR
evalGBMKmeansResults <- evalGBMKmeans(x = localDataGbm, 
                                      resp = "ch4.trate.mg.h_Estimate",
                                      covar = allCovar, 
                                      weights = 1/localDataGbm$ch4.trate.mg.h_StdError^2,
                                      nTrees = 20000, shr = 0.0005, bf = 0.9,
                                      cvFoldsMin = 2, cvFoldsMax = 10,
                                      trainPropMin = 0.5, trainPropMax = 0.9,
                                      nGridCv = 5, nGridProp = 5, nGBM = 50, setSeed = 2345)


write.table(evalGBMKmeansResults, file = "ohio2016/output/evalGBMKmeansResults.txt")