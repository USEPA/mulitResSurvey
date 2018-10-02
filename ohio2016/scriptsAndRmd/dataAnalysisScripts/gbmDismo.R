# Fitting GBM using dismo library.  This allows for use of gbm.simplify function.
# gbm.simplify doesn't work.  Too few observations.

resp <- "ch4.trate.mg.h_Estimate"

set.seed(2385) # for reproducability

# Data sampled evenly across 5 quantile groups.
# p=0.8 results in 27 samples, same as randomly sampling with
# 90% of data (see line 39)
# see https://topepo.github.io/caret/data-splitting.html#simple-splitting-based-on-the-outcome

trainInds <- caret::createDataPartition(localDataGbm2$group, p = 0.8, 
                                        list = FALSE, 
                                        times = 1)
tmpTrain <- localDataGbm[trainInds,]
tmpWeights <- localWeights[trainInds]
tmpTest <- localDataGbm[-trainInds,]

tmpGbm.d <- gbm.step(data = tmpTrain, 
              family = "gaussian",
              gbm.y = 2,
              gbm.x = c(15:27, 30),
              #formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
              site.weights = tmpWeights,
              max.trees = 20000,
              # n.minobsinnode = 2, # argument not recognized in this function
              #interaction.depth = 2,
              tree.complexity = 2,
              learning.rate = 0.0005, # small value
              bag.fraction = 0.9, # large value
              #cv.folds = 10, #
              n.folds = 10)

tmpGbm.d.simp <- gbm.simplify(tmpGbm.d, n.drops = 1, n.folds = 3, )

# This give error: "The dataset size is too small or subsampling rate is too 
# large: nTrain*bag.fraction <= n.minobsinnode"

tmpGbm.d$nTrain * tmpGbm.d$bag.fraction # this is 24.3.  
tmpGbm.d$n.minobsinnode # 10

# the dataset and parameter values do not violate listed criteria.  Hmm...?
# I can't seem to get the gbm.simplify function to work with this dataset.

tmpGbm.d.int <- gbm.interactions(tmpGbm.d)
tmpGbm.d.int$interactions
tmpGbm.d.int$rank.list
# no interactions identified


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# Is stochasticity in gbm.step as great as in gbm?


parmGrid.dismo <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                                isR2 = as.integer(rep(NA, 50)),
                                osMSE = as.numeric(rep(NA, 50)),
                                osR2 = as.integer(rep(NA, 50)),
                                optTrees = as.integer(rep(NA, 50)))

## Loop to execute 50 replicate runs.
set.seed(2385)# set at begining of loop for reproducability
for (i in 1:10) { #  min
  # New training data for each run
  trainInds <- sample(1:nrow(localDataGbm), 
                      floor(nrow(localDataGbm)*0.9)) # can change this
  tmpTrain <- localDataGbm[trainInds,]
  tmpWeights <- localWeights[trainInds]
  tmpTest <- localDataGbm[-trainInds,]
  
  tmpGbm.d <- gbm.step(data = tmpTrain, 
                      family = "gaussian",
                      gbm.y = 2,
                      gbm.x = c(15:27, 30),
                      #formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                      site.weights = tmpWeights,
                      max.trees = 20000,
                      # n.minobsinnode = 2, # argument not recognized in this function
                      #interaction.depth = 2,
                      tree.complexity = 2,
                      learning.rate = 0.0005, # small value
                      bag.fraction = 0.9, # large value
                      #cv.folds = 10, #
                      n.folds = 10)
  
  optTrees <- tmpGbm.d$gbm.call$best.trees
  
  ## MSE
  isPreds <-  tmpGbm.d$fitted
  parmGrid.dismo$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  parmGrid.dismo$isR2[i] <- summary(lm(isPreds ~ tmpTrain[,resp]))$r.squared
  
  osPreds <- predict.gbm(tmpGbm.d, newdata = tmpTest, n.trees = optTrees) 
  parmGrid.dismo$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
  parmGrid.dismo$osR2[i] <- summary(lm(osPreds ~ tmpTest[,resp]))$r.squared
  
  parmGrid.dismo$optTrees[i] <- optTrees
}
parmGrid.dismo # each runs has different MSE
parmGrid.dismo <- parmGrid.dismo %>% # an index for optimal # of trees
  mutate(iOptTrees = ifelse(optTrees == 20000,
                            "20000",
                            ifelse(optTrees < 200,
                                   "<200",
                                   ">200 - < 9,900")))
parmGrid.dismo.m <- melt(parmGrid.dismo)

# Reproducability between dismo gbms is no better than between
# gbm gbms.
ggplot(filter(parmGrid.dismo.m, grepl("MSE", variable)), 
       aes(variable, value)) + 
  geom_point() +
  ylab("MSE") +
  ylim(c(0,115)) +
  ggtitle("10 RUNS \nOPTIMAL bf AND shr") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 

