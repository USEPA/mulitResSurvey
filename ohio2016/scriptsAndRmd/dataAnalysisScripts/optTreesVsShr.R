# Inspection of evalGbm objects indicates that models with few trees (i.e.
# < 1500 have poorer predictive performance)

# Question 1: is cv folds argument required to generate an estimate of 
# optimal number of trees? 
# Answer: Yes, otherwise optimal number of trees is resturned with warning.

# Question 2: When using optimal parameters from evalGBM, we sometimes
# end up with brt models that have few trees (<1500) which seem to have
# worse performance than those with more trees.  Can we define a shrinkage
# rate that almost guarntees > ~2500 trees?

# Total CH4 emission rate---------------------------------
resp = "ch4.trate.mg.h_Estimate"
localDataGbmIndex <- grepl(pattern = "EPA", x = meanVariance.c.lake.lu.agg$citation)
localDataGbm <- meanVariance.c.lake.lu.agg[localDataGbmIndex, ]
localWeights = 1/localDataGbm$ch4.trate.mg.h_StdError^2 

set.seed(2385)
# Use same training and test data
trainInds <- sample(1:nrow(localDataGbm), 
                    floor(nrow(localDataGbm)*0.9))
tmpTrain <- localDataGbm[trainInds,]
tmpTest <- localDataGbm[-trainInds,]
tmpWeights <- localWeights[trainInds]

# 5 values for shrinkage rate
parmGrid <- data.frame(shr = rep(seq(from = 10e-6, to = 0.005, length.out = 5), each = 20), 
                       bf = 0.85)
parmGrid$isMSE <- as.numeric(NA)
parmGrid$osMSE <- as.numeric(NA)
parmGrid$optTrees <- as.integer(NA)

## Loop
for (i in 1:nrow(parmGrid)) {
tmpGbmCv <- gbm(data = tmpTrain, 
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = parmGrid$shr[i],
                bag.fraction = parmGrid$bf[i],
                cv.folds = 10)

## MSE
isPreds <- predict.gbm(tmpGbmCv, newdata = tmpTrain, n.trees = optTrees)
parmGrid$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)

osPreds <- predict.gbm(tmpGbmCv, newdata = tmpTest, n.trees = optTrees)
parmGrid$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 

parmGrid$optTrees[i] <- gbm.perf(tmpGbmCv)
}
