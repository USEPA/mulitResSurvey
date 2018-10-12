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
  
  # data
  x <- x[, c(resp, covar)] # remove uneeded columns,  Must so this before kMeans
  
  ## Loop
  set.seed(setSeed)
  for(i in 1:nrow(parmGrid)){
    message(i) # report number of iterations

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
  return(parmGrid)
}

# RUN SIMULATIONS WITH TOTAL CH4 EMISSION RATE, LOCAL OBSERVATIONS, ALLCOVAR-----
evalGBMKmeansResults <- evalGBMKmeans(x = localDataGbm, 
                                      resp = "ch4.trate.mg.h_Estimate",
                                      covar = allCovar, 
                                      weights = 1/localDataGbm$ch4.trate.mg.h_StdError^2,
                                      nTrees = 20000, shr = 0.0005, bf = 0.9,
                                      cvFoldsMin = 2, cvFoldsMax = 10,
                                      trainPropMin = 0.5, trainPropMax = 0.9,
                                      nGridCv = 5, nGridProp = 5, nGBM = 50, setSeed = 2345)


# # write.table(evalGBMKmeansResults, file = "ohio2016/output/evalGBMKmeansResults.txt")
# 
# # Read into workspace if needed
# evalGBMKmeansResults <- read.table(file = "ohio2016/output/evalGBMKmeansResults.txt")
# str(evalGBMKmeansResults)


# PLOTS----------------
# osMSE as a function of trainProp and cv folds
ggplot(evalGBMKmeansResults, aes(cvFolds,osMSE)) + 
  geom_point() +
  facet_wrap(~trainProp, labeller = label_both)

ggsave("ohio2016/output/figures/evalGBMKmeansTrainProp.tiff",
       units="in",  # specify units for dimensions
       width=11,   
       height=8, 
       dpi=600,   
       compression = "lzw")

# Distribution of osMSE at cvFolds == 10, trainProp == 0.9
p1 <- ggplot(filter(evalGBMKmeansResults, cvFolds == 10, trainProp == 0.9),
             aes(osMSE)) +
  geom_density() +
  ggtitle("shr=0.0005, bf=10, cv=10 \ntrainProp = 0.9, kMeansCluster") +
  theme(plot.title = element_text(size = 8))

p2 <- ggplot(filter(evalGBMKmeansResults, cvFolds == 10, trainProp == 0.9),
             aes(osMSE)) +
  stat_ecdf(geom = "step") +
  ylab("cumulative distribution") +
  ggtitle("shr=0.0005, bf=10, cv=10 \ntrainProp = 0.9, kMeansCluster") +
  theme(plot.title = element_text(size = 8))

p3 <- ggplot(filter(evalGBMKmeansResults, cvFolds == 10, trainProp %in% c(0.5, 0.9)),
       aes(osMSE, group = trainProp)) +
  stat_ecdf(aes(color = as.factor(trainProp))) +
  ylab("cumulative distribution") +
  ggtitle("shr=0.0005, bf=10, cv=10 \ntrainProp = 0.9, kMeansCluster") +
  theme(legend.position = c(0.8, 0.3),
        plot.title = element_text(size = 8)) +
  scale_color_discrete(name = "trainProp")


# Push to .tiff
tiff("ohio2016/output/figures/osMseDistribution.tiff",
     units = "in",
     width = 8,
     height = 3,
     res = 800,
     compression = "lzw")

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = vplayout(1,1))
print(p2, vp = vplayout(1,2))
print(p3, vp = vplayout(1,3))
dev.off()
  
  
  
# How does distribution of optTrees look?
ggplot(evalGBMKmeansResults, aes(optTrees,osMSE)) + 
  geom_point() +
  facet_wrap(~trainProp, labeller = label_both)
