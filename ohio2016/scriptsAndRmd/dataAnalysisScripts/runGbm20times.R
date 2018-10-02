## BACKGROUND------
## This script is used to identify important sources of stochasticity in the 
## gbm runs.  Stochasticity is causing disagreements between gbm runs using
## identical values for shr and bf.

## The approach here is to start with a model that has minimum/no stochasticity,
## then incrementally introduce elements of stochasticity.  At each step
## variation among 'replicate' runs will be assessed.  The goal is to identify
## total model stochasticity to individual model components.  Model components
## to be evaluated include training data, bagging fraction, seed, and cross
## validation.

## Background
## evalGBM was run to generate gbm models across a range of bf and shr values.
## Each model was run multiple times (10-50) for each combination of shr and bf.
## The average osMSE and isMSE was extracted for each simulation.  Differences
## among 'replicate' runs for a given pair of bf and shr values represent 
## stochasticity from the randomly chosen training data and the bf.  

## The runGBM function identifies the set of bf and shr values that minimize the 
## osMSE (on average, across the replicate runs for each combination of bf and shr).  
## runGBM.v2 is a tweak to the original runGBM that allows the function to call
## results from evalGBM where the nGBM argument is set to something other than
## the default value of 10.

## FIRST RUN USES PARAMETER VALUES FROM evalGBM WITH ALL SOURCES OF STOCHASTICITY----

# Create parmGrid
parmGrid.original <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                                isR2 = as.integer(rep(NA, 50)),
                                osMSE = as.numeric(rep(NA, 50)),
                                osR2 = as.integer(rep(NA, 50)),
                                optTrees = as.integer(rep(NA, 50)))

## Loop to execute 50 replicate runs.
set.seed(2385)# set at begining of loop for reproducability
for (i in 1:50) { #  min
  # New training data for each run
  trainInds <- sample(1:nrow(localDataGbm), 
                      floor(nrow(localDataGbm)*0.9)) # can change this
  tmpTrain <- localDataGbm[trainInds,]
  tmpWeights <- localWeights[trainInds]
  tmpTest <- localDataGbm[-trainInds,]
  
  tmpGbm <- gbm(data = tmpTrain, # different training data ecah iteration of loop
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = 0.0028, # optimal value from nGBM = 50, seed = 5679
                bag.fraction = 0.72, # optimal value from evalGBM
                cv.folds = 10) # 10-fold cross validation
  
  optTrees <- gbm.perf(tmpGbm) 
  
  ## MSE
  isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = optTrees) 
  parmGrid.original$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  parmGrid.original$isR2[i] <- summary(lm(isPreds ~ tmpTrain[,resp]))$r.squared
  
  osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = optTrees) 
  parmGrid.original$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
  parmGrid.original$osR2[i] <- summary(lm(osPreds ~ tmpTest[,resp]))$r.squared
  
  parmGrid.original$optTrees[i] <- optTrees
}
parmGrid.original # each runs has different MSE
parmGrid.original <- parmGrid.original %>% # an index for optimal # of trees
  mutate(iOptTrees = ifelse(optTrees > 9900,
                            ">9,900",
                            ifelse(optTrees < 200,
                                   "<200",
                                   ">200 - < 9,900")))
parmGrid.original.m <- melt(parmGrid.original)

# 
ggplot(filter(parmGrid.original.m, grepl("MSE", variable)), 
                     aes(variable, value)) + 
  geom_point() +
  ylab("MSE") +
  ylim(c(0,115)) +
  ggtitle("50 RUNS \nOPTIMAL bf AND shr") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 

ggsave(filename = "ohio2016/output/figures/tch4OptimalParameters.tiff",
       units = "in",
       width = 3.5,
       height = 4,
       dpi = 800,
       compression = "lzw")

ggplot(parmGrid.original, aes(osMSE, osR2)) +
  geom_point()


## SECOND SIMULATION CONTROLS FOR ALL STOCHASTICITY-----------------

# bag.fraction = 0, CV = 0, same training data.  optTrees can't be calculated
# under these conditions, therefore need to give values for n.tree in predict.gbm()

# Set up for exploratory gbm runs-
resp = "ch4.trate.mg.h_Estimate"
localDataGbmIndex <- grepl(pattern = "EPA", x = meanVariance.c.lake.lu.agg$citation)
localDataGbm <- meanVariance.c.lake.lu.agg[localDataGbmIndex, ]
localWeights = 1/localDataGbm$ch4.trate.mg.h_StdError^2 


# Identify training and test data
set.seed(2385) # for reproducability
trainInds <- sample(1:nrow(localDataGbm), 
                    floor(nrow(localDataGbm)*0.9)) # can change this
tmpTrain <- localDataGbm[trainInds,]
tmpWeights <- localWeights[trainInds]
tmpTest <- localDataGbm[-trainInds,]

# Grid for holding model results
parmGrid <- data.frame(isMSE =  as.numeric(rep(NA, 20)),
                       osMSE = as.numeric(rep(NA, 20)),
                       optTrees = as.integer(rep(NA, 20)))

## Loop to execute 20 replicate runs.
for (i in 1:20) { # FAST
  # set.seed(2385)
  tmpGbm <- gbm(data = tmpTrain, # training data remains constant
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = 0.0028, # optimal value from nGBM = 50, seed = 5679
                bag.fraction = 1, # no bagging
                cv.folds = 0) # no cross validation
  optTrees <- NA # can't execute gbm.perf() when BF=1 and CV = 0
  
  ## MSE
  isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = 8000) # Can't calculate optTrees, so give a number.
  parmGrid$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  
  osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = 8000) # Can't calculate optTrees, so give a number.
  parmGrid$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
  
  parmGrid$optTrees[i] <- optTrees
}

parmGrid # all 20 runs are identical. good!

## THIRD SIMULATION ALLOWS FOR STOCHASTICITY FROM BAGGING-----------------

# Create parmGrid
parmGrid.bf <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                       osMSE = as.numeric(rep(NA, 50)),
                       optTrees = as.integer(rep(NA, 50)))

## Loop to execute 50 replicate runs.
set.seed(2385) #2385 set at begining of loop for reproducability
for (i in 1:50) { # 2 min for 50 simulations
  # set.seed(2385) all runs identical if seed is set
  tmpGbm <- gbm(data = tmpTrain, # training data remains constant
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = 0.0028, # optimal value from nGBM = 50, seed = 5679
                bag.fraction = 0.75, # bagging allowed
                cv.folds = 0) # no cross validation
  
  optTrees <- gbm.perf(tmpGbm) # gives warning
  
  ## MSE
  isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = optTrees) 
  parmGrid.bf$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  
  osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = optTrees) 
  parmGrid.bf$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
  
  parmGrid.bf$optTrees[i] <- optTrees
}
parmGrid.bf # each runs has different MSE
parmGrid.bf.m <- melt(parmGrid.bf)

# 
p.bag <- ggplot(filter(parmGrid.bf.m, grepl("MSE", variable)), 
                aes(variable, value)) + 
  geom_point() +
  ylab("MSE") +
  ylim(c(0,115)) +
  ggtitle("BAGGING") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 

## FOURTH SIMULATION ALLOWS FOR STOCHASTICITY FROM CROSS VALIDATION-----------------

# Create parmGrid
parmGrid.cv <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                          osMSE = as.numeric(rep(NA, 50)),
                          optTrees = as.integer(rep(NA, 50)))

## Loop to execute 50 replicate runs.
set.seed(2385)# set at begining of loop for reproducability
for (i in 1:50) { # Cross validation extends run time.  8 min

  tmpGbm <- gbm(data = tmpTrain, # training data remains constant
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = 0.0028, # optimal value from nGBM = 50, seed = 5679
                bag.fraction = 1, # no bagging
                cv.folds = 10) # 10-fold cross validation
  
  optTrees <- gbm.perf(tmpGbm) 
  
  ## MSE
  isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = optTrees) 
  parmGrid.cv$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  
  osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = optTrees) 
  parmGrid.cv$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
  
  parmGrid.cv$optTrees[i] <- optTrees
}
parmGrid.cv # each runs has different MSE
parmGrid.cv <- parmGrid.cv %>% # an index for optimal # of trees
  mutate(iOptTrees = ifelse(optTrees > 9900,
                           ">9,900",
                           ifelse(optTrees < 200,
                                  "<200",
                                  ">200 - < 9,900")))
parmGrid.cv.m <- melt(parmGrid.cv)


# 
p.cv <- ggplot(filter(parmGrid.cv.m, grepl("MSE", variable)), aes(variable, value)) + 
  geom_point(aes(color = iOptTrees)) +
  # geom_point()
  ylab("MSE") +
  ylim(c(0,115)) +
  ggtitle("CROSS VALIDATION") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.justification = c(0,1),
        legend.position = c(0,1)) 

## FIFTH SIMULATION ALLOWS FOR STOCHASTICITY FROM DIFFERENT TRAINING DATA------

# Create parmGrid
parmGrid.data <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                          osMSE = as.numeric(rep(NA, 50)),
                          optTrees = as.integer(rep(NA, 50)))

## Loop to execute 50 replicate runs.
set.seed(2385)# set at begining of loop for reproducability
for (i in 1:50) { #  min
  # New training data for each run
  trainInds <- sample(1:nrow(localDataGbm), 
                      floor(nrow(localDataGbm)*0.9)) # can change this
  tmpTrain <- localDataGbm[trainInds,]
  tmpWeights <- localWeights[trainInds]
  tmpTest <- localDataGbm[-trainInds,]
  
  tmpGbm <- gbm(data = tmpTrain, # different training data ecah iteration of loop
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = 0.0028, # optimal value from nGBM = 50, seed = 5679
                bag.fraction = 1, # no bagging
                cv.folds = 0) # no cross validation
  
  optTrees <- NA # can't execute gbm.perf() when BF=1 and CV = 0
  
  ## MSE
  isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = 1000) 
  parmGrid.data$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  
  osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = 1000) 
  parmGrid.data$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
  
  parmGrid.data$optTrees[i] <- optTrees
}
parmGrid.data # each runs has different MSE
parmGrid.data.m <- melt(parmGrid.data)

# 
p.data <- ggplot(filter(parmGrid.data.m, grepl("MSE", variable)), aes(variable, value)) + 
  geom_point() +
  ylab("MSE") +
  ylim(c(0,115)) +
  ggtitle("TRAINING DATA \noptTrees = 1000") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 


## SIXTH SIMULATION ATTEMPTS TO REDUCE STOCHASTICITY ASSOCIATED WITH------
## DIFFERENT TRAINING SETS BY CREATING MORE REPRESENTATIVE SUBSETS OF
## ENTIRE DATA SET

# Group observations by quantile
localDataGbm2 <- localDataGbm %>%
  mutate(group = cut(ch4.trate.mg.h_Estimate,
                     breaks = quantile(ch4.trate.mg.h_Estimate, 
                                       probs = seq(0, 1, by = 0.20)),
                     include.lowest = TRUE,
                     labels = c("0-0.2", "0.2-0.4",
                                "0.4-0.6", "0.6-0.8",
                                "0.8-1.0")) %>%
           as.character()) # convert factor to chr, else cv fails

# Create parmGrid
parmGrid.data2 <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                             osMSE = as.numeric(rep(NA, 50)),
                             optTrees = as.integer(rep(NA, 50)))

## Loop to execute 20 replicate runs.
set.seed(2385)# set at begining of loop for reproducability
for (i in 1:50) { #  min
  # New training data for each run
  # Data sampled evenly across 5 quantile groups.
  # p=0.8 results in 27 samples, same as randomly sampling with
  # 90% of data (see line 39)
  # see https://topepo.github.io/caret/data-splitting.html#simple-splitting-based-on-the-outcome
  
  trainInds <- caret::createDataPartition(localDataGbm2$group, p = 0.8, 
                                          list = FALSE, 
                                          times = 1)
  tmpTrain <- localDataGbm2[trainInds,]
  tmpWeights <- localWeights[trainInds]
  tmpTest <- localDataGbm2[-trainInds,]
  
  tmpGbm <- gbm(data = tmpTrain, # different training data each iteration of loop
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = 0.0028, # optimal value from nGBM = 50, seed = 5679
                bag.fraction = 1, # no bagging
                cv.folds = 0) # No cross validation
  
  optTrees <- NA # can't execute gbm.perf() when BF=1 and CV = 0
  
  ## MSE
  isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = 1000) 
  parmGrid.data2$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  
  osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = 1000) 
  parmGrid.data2$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
  
  parmGrid.data2$optTrees[i] <- optTrees
}
parmGrid.data2 # each runs has different MSE
parmGrid.data2.m <- melt(parmGrid.data2)

# 
p.data2 <- ggplot(filter(parmGrid.data2.m, grepl("MSE", variable)), aes(variable, value)) + 
  geom_point() +
  ylab("MSE") +
  ylim(c(0,115)) +
  ggtitle("REPRESENTATIVE TRAINING DATA \noptTree = 1000") +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 





## PUSH SIMULATION RESULTS TO MULTI-PANEL FIGURE-------------------
tiff("ohio2016/output/figures/partitionStochasticity.tiff",
     units = "in",
     width = 7,
     height = 8,
     res = 800,
     compression = "lzw")

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(p.bag, vp = vplayout(1,1))
print(p.cv, vp = vplayout(1,2))
print(p.data, vp = vplayout(2,1))
print(p.data2, vp = vplayout(2,2))
dev.off()




## SEVENTH SIMULATION ALLOWS FOR STOCHASTICITY FROM DIFFERENT TRAINING DATA------
## AND CV.  ALLOWS FOR CALCULATION OF optTREES.

# Create parmGrid
parmGrid.cv.data <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                            osMSE = as.numeric(rep(NA, 50)),
                            optTrees = as.integer(rep(NA, 50)))

## Loop to execute 50 replicate runs.
set.seed(2385)# set at begining of loop for reproducability
for (i in 1:50) { #  min
  # New training data for each run
  trainInds <- sample(1:nrow(localDataGbm), 
                      floor(nrow(localDataGbm)*0.9)) # can change this
  tmpTrain <- localDataGbm[trainInds,]
  tmpWeights <- localWeights[trainInds]
  tmpTest <- localDataGbm[-trainInds,]
  
  tmpGbm <- gbm(data = tmpTrain, # different training data ecah iteration of loop
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = 0.0028, # optimal value from nGBM = 50, seed = 5679
                bag.fraction = 1, # no bagging
                cv.folds = 10) # no cross validation
  
  optTrees <- gbm.perf(tmpGbm) 
  
  ## MSE
  isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = optTrees) 
  parmGrid.cv.data$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  
  osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = optTrees) 
  parmGrid.cv.data$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
  
  parmGrid.cv.data$optTrees[i] <- optTrees
}
parmGrid.cv.data # each runs has different MSE
parmGrid.cv.data <- parmGrid.cv.data %>% # an index for optimal # of trees
  mutate(iOptTrees = ifelse(optTrees > 9900,
                            ">9,900",
                            ifelse(optTrees < 200,
                                   "<200",
                                   ">200 - < 9,900")))
parmGrid.cv.data.m <- melt(parmGrid.cv.data)

# 
p.data.cv <- ggplot(filter(parmGrid.cv.data.m, grepl("MSE", variable)), aes(variable, value)) + 
  geom_point(aes(color = iOptTrees)) +
  ylab("MSE") +
  ylim(c(0,115)) +
  ggtitle("TRAINING DATA + CV") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.background = element_blank()) 


## EIGTH SIMULATION ATTEMPTS TO REDUCE STOCHASTICITY ASSOCIATED WITH------
## DIFFERENT TRAINING SETS BY CREATING MORE REPRESENTATIVE SUBSETS OF
## ENTIRE DATA SET.  THIS RUN ALSO INCORPORATES CV.

# Group observations by quantile
localDataGbm2 <- localDataGbm %>%
  mutate(group = cut(ch4.trate.mg.h_Estimate,
                     breaks = quantile(ch4.trate.mg.h_Estimate, 
                                       probs = seq(0, 1, by = 0.20)),
                     include.lowest = TRUE,
                     labels = c("0-0.2", "0.2-0.4",
                                "0.4-0.6", "0.6-0.8",
                                "0.8-1.0")) %>%
           as.character())

# Create parmGrid
parmGrid.cv.data2 <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                             osMSE = as.numeric(rep(NA, 50)),
                             optTrees = as.integer(rep(NA, 50)))

## Loop to execute 50 replicate runs.
set.seed(2385)# set at begining of loop for reproducability
for (i in 1:50) { #  12 min
  # New training data for each run
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
  
  tmpGbm <- gbm(data = tmpTrain, # different training data each iteration of loop
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = 0.0028, # optimal value from nGBM = 50, seed = 5679
                bag.fraction = 1, # no bagging
                cv.folds = 10) # 
  
  optTrees <- gbm.perf(tmpGbm) # 
  
  ## MSE
  isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = optTrees) 
  parmGrid.cv.data2$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  
  osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = optTrees) 
  parmGrid.cv.data2$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
  
  parmGrid.cv.data2$optTrees[i] <- optTrees
}
parmGrid.cv.data2 # each runs has different MSE
parmGrid.cv.data2 <- parmGrid.cv.data2 %>% # an index for optimal # of trees
  mutate(iOptTrees = ifelse(optTrees > 9900,
                            ">9,900",
                            ifelse(optTrees < 200,
                                   "<200",
                                   ">200 - < 9,900")))
parmGrid.cv.data2.m <- melt(parmGrid.cv.data2)

# 
p.data.cv2 <- ggplot(filter(parmGrid.cv.data2.m, grepl("MSE", variable)), aes(variable, value)) + 
  geom_point(aes(color = iOptTrees)) +
  ylab("MSE") +
  ylim(c(0,115)) +
  ggtitle("REPRESENTATIVE TRAINING DATA + CV") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.background = element_blank())

## PUSH TRAINING DATA + CV TO TIFF----
tiff("ohio2016/output/figures/partitionStochasticityTrainDataCv.tiff",
     units = "in",
     width = 7,
     height = 8,
     res = 800,
     compression = "lzw")

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(p.data, vp = vplayout(1,1))
print(p.data2, vp = vplayout(1,2))
print(p.data.cv, vp = vplayout(2,1))
print(p.data.cv2, vp = vplayout(2,2))
dev.off()

## NINTH SIMULATION: REPRESENTATIVE TRAINING DATA SET + CV + BAG----
# Create parmGrid
parmGrid.cv.data2.bag <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                                    osMSE = as.numeric(rep(NA, 50)),
                                    optTrees = as.integer(rep(NA, 50)))

## Loop to execute 50 replicate runs.
set.seed(2385)# set at begining of loop for reproducability
for (i in 1:50) { #  12 min
  # New training data for each run
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
  
  tmpGbm <- gbm(data = tmpTrain, # different training data each iteration of loop
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = 0.0028, # optimal value from nGBM = 50, seed = 5679
                bag.fraction = 0.9, # 
                cv.folds = 10) # 
  
  optTrees <- gbm.perf(tmpGbm) # 
  
  ## MSE
  isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = optTrees) 
  parmGrid.cv.data2.bag$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  
  osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = optTrees) 
  parmGrid.cv.data2.bag$osMSE[i] <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest) 
  
  parmGrid.cv.data2.bag$optTrees[i] <- optTrees
}
parmGrid.cv.data2.bag # each runs has different MSE
parmGrid.cv.data2.bag <- parmGrid.cv.data2.bag %>% # an index for optimal # of trees
  mutate(iOptTrees = ifelse(optTrees > 9900,
                            ">9,900",
                            ifelse(optTrees < 200,
                                   "<200",
                                   ">200 - < 9,900")))
parmGrid.cv.data2.bag.m <- melt(parmGrid.cv.data2.bag)

# 
p.data2.cv2 <- ggplot(filter(parmGrid.cv.data2.bag.m, grepl("MSE", variable)), aes(variable, value)) + 
  geom_point() +
  ylab("MSE") +
  ylim(c(0,115)) +
  ggtitle("REPRESENTATIVE TRAINING DATA + CV \n+BAGGING") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))


p.data2.cv2.mse <- ggplot(filter(parmGrid.cv.data2.bag.m, grepl("osMSE", variable)), 
                          aes(iOptTrees, value)) + 
  geom_point() +
  ylab("MSE") +
  ylim(c(0,115)) +
  ggtitle("REPRESENTATIVE TRAINING DATA + CV \n+BAGGING") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))


# Push to .tiff
tiff("ohio2016/output/figures/stochasticityTrainDataCv.tiff",
     units = "in",
     width = 7,
     height = 4,
     res = 800,
     compression = "lzw")

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(p.data2.cv2, vp = vplayout(1,1))
print(p.data2.cv2.mse, vp = vplayout(1,2))
dev.off()




## TENTH RUN: GBM RUN WITH 'RULE OF THUMB' PARAMETERS: PLUS REPRESENTATIVE----
## TRAINING DATA SET + CV + BAG
# Create parmGrid
parmGrid.cv.data2.bag <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                                    osMSE = as.numeric(rep(NA, 50)),
                                    optTrees = as.integer(rep(NA, 50)))


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
  
  tmpGbm <- gbm(data = tmpTrain, # different training data each iteration of loop
                distribution = "gaussian",
                formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                weights = tmpWeights,
                n.trees = 10000,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = 0.0005, # small value
                bag.fraction = 0.9, # large value
                cv.folds = 10) # 
  
  optTrees <- gbm.perf(tmpGbm) # 
  
  ## MSE
  isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = optTrees)
  mse_is <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)
  osPreds <- predict.gbm(tmpGbm, newdata = tmpTest, n.trees = optTrees)
  mse_os <- sum(c(osPreds - tmpTest[,resp])^2)/nrow(tmpTest)
  mse <- data.frame(msetype = c("in_sample", "out_sample"),
                    mse_value = c(mse_is, mse_os))
  rsq_is <- summary(
    lm(isPreds ~ tmpTrain[,resp]))$r.squared
  
  ## Prediction plot
  tmpTrain <- mutate(tmpTrain, isPreds = isPreds)

  # In sample observed vs predicted  
p.pred.obs <- ggplot(tmpTrain, aes_string(resp, isPreds)) + 
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    ylab("predicted") +
    xlim(range(c(select(tmpTrain, eval(resp)) , 
                 tmpTrain$isPreds))) + # equal range x/y axis
    ylim(range(c(select(tmpTrain, eval(resp)) , 
                 tmpTrain$isPreds))) + # equal range x/y axis
    ggtitle(paste(obs, gasNm, gasSrc, "~", covarType, "\n",
                  "mse_is =", round(mse_is, 1),
                  "  mse_os =", round(mse_os, 1), "\n",
                  "optTrees =", optTrees, "\n",
                  "r2 =", rsq_is)) +
    theme(plot.title = element_text(size = 12))

## Relative influence plot
  relInfDf <- tmpGbm %>% summary(plotit = FALSE) %>% filter(rel.inf >= 5)
  relInfPlot <- ggplot(relInfDf, aes(x = reorder(var, rel.inf), y=rel.inf)) + geom_bar(stat = "identity") +
    coord_flip() + ylab("Relative Influence (0-100)") + xlab("Variable") +
    ggtitle(resp) +
    theme(plot.title = element_text(size = 10))
  
  tiff("ohio2016/output/figures/tCh4Preliminary.tiff",
       units = "in",
       width = 7,
       height = 4,
       res = 800,
       compression = "lzw")
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,2)))
  
  vplayout <- function(x,y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  print(p.pred.obs, vp = vplayout(1,1))
  print(relInfPlot, vp = vplayout(1,2))
  dev.off()  
  
  ## Partial dependence plots
  covs <- as.character(relInfDf$var) # 6 most important variables
  numPlots <- length(covs)
  pl <- list()
  for(i in 1:numPlots){
    # i = 1
    # 
    p <- partial(tmpGbm, pred.var = covs[i], train = tmpTrain, n.trees = optTrees)
    f <- plotPartial(p, smooth = TRUE, lwd = 2)
    pl[[i]] <- f
  }
  p1 <- gridExtra::marrangeGrob(pl, nrow = 3, ncol = 2) # push 6 plots to one page

  tiff("ohio2016/output/figures/tCh4PreliminaryPdPlot.tiff",
       units = "in",
       width = 8,
       height = 11,
       res = 800,
       compression = "lzw")
 p1
  dev.off()  

  
  
  
  
  
  
  
  
  
  
## ELEVENTH SIMULATION: ALL DATA, 'RULE OF THUMB' PARAMETERS, BAG, CV---------
  # Create parmGrid
  parmGrid.all.data <- data.frame(isMSE =  as.numeric(rep(NA, 50)),
                                      optTrees = as.integer(rep(NA, 50)))
  
  ## Loop to execute 50 replicate runs.
  set.seed(2385)# set at begining of loop for reproducability
  for (i in 1:50) { #  12 min
    # All data for each run

    tmpTrain <- localDataGbm
    tmpWeights <- localWeights

    tmpGbm <- gbm(data = tmpTrain, # different training data each iteration of loop
                  distribution = "gaussian",
                  formula = as.formula(paste(resp,"~",paste(allCovar, collapse="+"))),
                  weights = tmpWeights,
                  n.trees = 10000,
                  n.minobsinnode = 2,
                  interaction.depth = 2,
                  shrinkage = 0.0005, # rule of thumb value
                  bag.fraction = 0.9, # 
                  cv.folds = 10) # 
    
    optTrees <- gbm.perf(tmpGbm) # 
    
    ## MSE
    isPreds <- predict.gbm(tmpGbm, newdata = tmpTrain, n.trees = optTrees) 
    parmGrid.all.data$isMSE[i] <- sum(c(isPreds - tmpTrain[,resp])^2)/nrow(tmpTrain)

    parmGrid.all.data$optTrees[i] <- optTrees
  }
  parmGrid.all.data # each runs has different MSE
  parmGrid.all.data <- parmGrid.all.data %>% # an index for optimal # of trees
    mutate(iOptTrees = ifelse(optTrees > 9900,
                              ">9,900",
                              ifelse(optTrees < 200,
                                     "<200",
                                     ">200 - < 9,900")))
  parmGrid.all.data.m <- melt(parmGrid.all.data)
  summary(tmpGbm)
  
  # 
  p.all.data <- ggplot(filter(parmGrid.all.data.m, grepl("MSE", variable)), aes(variable, value)) + 
    geom_point() +
    ylab("MSE") +
    ylim(c(0,115)) +
    ggtitle("REPRESENTATIVE TRAINING DATA + CV \n+BAGGING") +
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  

  
  
  # Push to .tiff
  tiff("ohio2016/output/figures/stochasticityTrainDataCv.tiff",
       units = "in",
       width = 7,
       height = 4,
       res = 800,
       compression = "lzw")
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,2)))
  
  vplayout <- function(x,y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  print(p.data2.cv2, vp = vplayout(1,1))
  print(p.data2.cv2.mse, vp = vplayout(1,2))
  dev.off()
  
  
  
  
# goodness of fit------------
gof <- lapply(gbmModTest,  function(x) {
  melt(x$mse) %>% 
    dcast(variable ~ msetype) %>%
    cbind(., x$rsq_is, x$optTrees)}) %>%
  do.call("rbind", .) %>%
  select(-variable) 
names(gof) = gsub(pattern = c("\\$|x"), replacement = "", x = names(gof))
gof.m <- melt(gof[, c("in_sample", "out_sample")])


# First, look at distribution of is and os MSE values
ggplot(gof.m, aes(variable, value)) + geom_point() +
  ylab("MSE") +
  theme(axis.title.x = element_blank())



ggplot(gof, aes(in_sample, rsq_is)) + geom_point()
ggplot(gof, aes(out_sample, rsq_is)) + geom_point()
ggplot(gof, aes(optTrees, in_sample)) + geom_point() +
  geom_vline(xintercept = 1000, color = "red")















####################################################################
# runGBM 20 times.  Each run will use the same values for bf and shr (optimal
# values extracted from evalGBM object), but a different seed and therefore 
# different training set. Additional stochastisity will be introduced via the bf. 

gbmModTest <- list()
for (i in 1:1) { # 1min 30sec for 1:2.
  # this was run for 1:20.  Gives 20 different seeds.
  gbmModTest[[i]] <- runGBM.v2(resp = "ch4.trate.mg.h_Estimate",
                               covarType = "Full", nTrees = 10000, 
                               seed = 2222 + i, # here i is used to generate different seeds.
                               file = "50.2856", 
                               obs = "Local")
}
# goodness of fit
gof <- lapply(gbmModTest,  function(x) {
  melt(x$mse) %>% 
    dcast(variable ~ msetype) %>%
    cbind(., x$rsq_is, x$optTrees)}) %>%
  do.call("rbind", .) %>%
  select(-variable) 
names(gof) = gsub(pattern = c("\\$|x"), replacement = "", x = names(gof))
gof.m <- melt(gof[, c("in_sample", "out_sample")])


# First, look at distribution of is and os MSE values
ggplot(gof.m, aes(variable, value)) + geom_point() +
  ylab("MSE") +
  theme(axis.title.x = element_blank())



ggplot(gof, aes(in_sample, rsq_is)) + geom_point()
ggplot(gof, aes(out_sample, rsq_is)) + geom_point()
ggplot(gof, aes(in_sample, optTrees)) + geom_point() +
  geom_hline(yintercept = 2000, color = "red")





####################################################################
# The above 'replicate' models showed a large range of performance metrics (i.e.
# osMSE, optTrees, etc).  I want to know if stochastisity is coming more from
# bf or train data.  The runs belos are idenical to those above, except that I
# set bf = 1 in runGBM function.  Stochasticity will be due entirely to train
# data.


# rerun with bf = 1.  Remember to change to original in runGBM.
gbmModTestBf1 <- list()
for (i in 1:20) { # 1min 30sec for 1:2.
  # this was run for 1:20.  Gives 20 different seeds.
  gbmModTestBf1[[i]] <- runGBM.v2(resp = "ch4.trate.mg.h_Estimate",
                               covarType = "Full", nTrees = 10000, 
                               seed = 2222 + i, # here i is used to generate different seeds.
                               file = "50.5679", 
                               obs = "Local")
}
# goodness of fit
gof <- lapply(gbmModTest,  function(x) {
  melt(x$mse) %>% 
    dcast(variable ~ msetype) %>%
    cbind(., x$rsq_is, x$optTrees)}) %>%
  do.call("rbind", .) %>%
  select(-variable) 
names(gof) = gsub(pattern = c("\\$|x"), replacement = "", x = names(gof))
gof.m <- melt(gof[, c("in_sample", "out_sample")])


# First, look at distribution of is and os MSE values
ggplot(gof.m, aes(variable, value)) + geom_point() +
  ylab("MSE") +
  theme(axis.title.x = element_blank())



ggplot(gof, aes(in_sample, rsq_is)) + geom_point()
ggplot(gof, aes(out_sample, rsq_is)) + geom_point()
ggplot(gof, aes(optTrees, in_sample)) + geom_point() +
  geom_vline(xintercept = 1000, color = "red")


