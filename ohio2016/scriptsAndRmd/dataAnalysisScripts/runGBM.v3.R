## Will wrote a nice function to run a gbm using best parameters extracted from
## objects produced with evalGBM function (evalGBM.R).  I modified this function
## to accomodate evalGBM objects created with nGBM set to 50 (runGBM.v2.R).
## Here I modify the function to accept default values for shr and bf, rather than
## extracting 'optimal' values from evalGBM.  I also use 'repeat' and 'break'
## to rerun gbm function until optimal number of trees > 1000 & < 9990.

## Function assumes 'localDataGbm' and 'natDataGbm' exist in environments (defined
## in evalGBM.R).  Function takes a response, a set of covariates, and a National/All
## designation.
runGBM.v3 <- function(resp, covarType = "All", obs, nTrees = 10000, 
                   seed = 2222, bf, shr){
  ## This script assumes that allCovar and nationalCovar (defineRespCov.R)
  ## objects are in the global environment.
  # resp = "ch4.trate.mg.h_Estimate"
  # covarType = "All" # 'All' or 'Nat'
  # obs = "Local" # 'Local' or 'Nat' using local observations or all data
  # seed = 2222
  # bf = 0.9 # bagging fraction
  # shr = 0.0005 # small number
  
  
  
  ## Run gbm model
  set.seed(seed) # for reproducability
  dataGbm <- eval(parse(text = paste0(tolower(obs), "DataGbm")))
  wts <- 1/dataGbm[,gsub("_Estimate","_StdError",resp)]^2
  respVec <- dataGbm[, resp] # response put in vector for 'cut' call
  dataGbm <- dataGbm %>% 
    mutate(group = cut(respVec, # create grouping variable for representative subset
                       breaks = quantile(respVec,
                                         probs = seq(0, 1, by = 0.20)),
                       include.lowest = TRUE,
                       labels = c("0-0.2", "0.2-0.4",
                                  "0.4-0.6", "0.6-0.8",
                                  "0.8-1.0")) %>%
             as.character()) # careful, group can't be factor, else
                             # messes up CV.
           
  trainInds <- caret::createDataPartition(dataGbm$group, p = 0.8, 
                                          list = FALSE, 
                                          times = 1)
  tmpTrain <- dataGbm[trainInds,]
  tmpWeights <- wts[trainInds]
  tmpTest <- dataGbm[-trainInds,]
  
  if(covarType == "All"){
    gbmFormula <- as.formula(paste(resp,"~",paste(allCovar, collapse="+")))
  }else{
    gbmFormula <- as.formula(paste(resp,"~",paste(nationalCovar, collapse="+")))
  }

  
  repeat{ # Repeat running gbm until...
  tmpGbm <- gbm(formula = gbmFormula, 
                distribution = "gaussian",
                data = tmpTrain,
                weights = tmpWeights,
                n.trees = nTrees,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = shr,
                bag.fraction = bf,  
                cv.folds = 10)
  optTrees <- gbm.perf(tmpGbm)
  if (optTrees > 1000 & optTrees < 9990){ # if good # of trees
    break # then break
  } # otherwise keep going
  }
  
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
 
  predPlot <- ggplot(tmpTrain, aes_string(resp, isPreds)) + 
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    ylab("predicted") +
    xlim(range(c(select(tmpTrain, eval(resp)) , 
                 tmpTrain$isPreds))) + # equal range x/y axis
    ylim(range(c(select(tmpTrain, eval(resp)) , 
                 tmpTrain$isPreds))) + # equal range x/y axis
    ggtitle(paste(obs, resp, "~", covarType, "\n",
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
  
  ## Partial dependence plots
  covs <- as.character(relInfDf$var)[1:6] # top 6
  covs <- covs[!is.na(covs)] # if <6 variables meet criteria line 95
  numPlots <- length(covs)
  pl <- list()
  for(i in 1:numPlots){
    # i = 1
    p <- partial(tmpGbm, pred.var = covs[i], train = tmpTrain, n.trees = optTrees)
    f <- plotPartial(p, smooth = TRUE, lwd = 2)
    pl[[i]] <- f
  }
  p1 <- gridExtra::marrangeGrob(pl, nrow = 3, ncol = 2)
  return(list("gbm"=tmpGbm,"optTrees"=optTrees, "mse" = mse, 
              "rsq_is" = rsq_is,
              "predPlot"=predPlot, "RelInfPlot"=relInfPlot,
              "PDPlots"=p1))
}
