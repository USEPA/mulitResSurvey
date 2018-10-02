## Will wrote a nice function to run a gbm using best parameters extracted from
## objects produced with evalGBM function (evalGBM.R).  This function fails when
## the objects are given names that don't adhere to a certain convention.  This
## convention was broken when we started playing with nGBM argument in evalGBM().
## This script started with Will's function, and was then modified to accept
## these objects.  

## Function assumes 'localDataGbm' and 'natDataGbm' exist in environments (defined
## in evalGBM.R).  Function takes a response, a set of covariates, and a National/Full
## designation - and finds the best parameters from previous simulations
runGBM.v2 <- function(resp, covarType = "Full", nTrees = 10000, 
                   seed = 2222, file, obs){
  ## This script assumes that covarList (defined in and nationalCovar
  ## objects are in the global environment.
  # resp = "ch4.trate.mg.h_Estimate"
  # covarType = "Full" # 'Full' or 'Nat'
  # seed = 2222
  # file = "50.2856"
  # obs = "Local" # 'Local' or 'Nat' using local observations or all data
  
  # Read in relevant R workspace.
  gasNm <- ifelse(grepl("ch4",resp), "Ch4", "Co2")
  gasSrc <- ifelse(grepl("trate",resp), "trate",
                   ifelse(grepl("erate", resp), "erate","drate"))
  rdNm <- paste("eval", obs, gasNm, gasSrc,
                ifelse(covarType == "Full","Full","Nat"),
                file, # nGBM setting in evalGBM run
                ".RData", sep="")

  # Object brought in will conform to convention 'evalLocalCh4trateFull'
  # regardless of name of file
  load(paste0("ohio2016/output/", rdNm))
  
  ## Assign the evalGbm returned object to a known named object.
  assign("evalGbmResults", eval(parse(text=gsub(paste0(file, ".RData"),"", rdNm))))
  
  ## Look for the best parameter set
  orderOSMSE <- order(evalGbmResults$parameterGrid$osMSE)
  orderOptTrees <- evalGbmResults$parameterGrid$optTrees[orderOSMSE]
  treesMaxInds <- which(abs(orderOptTrees-10000)<10)
  if(length(treesMaxInds)>0) orderOSMSE <- orderOSMSE[-treesMaxInds]
  bestPars <- evalGbmResults$parameterGrid[orderOSMSE[1],] # '1' is location of min OSMSE
  
  ## Run gbm model
  set.seed(seed)
  dataGbm <- eval(parse(text = paste0(tolower(obs), "DataGbm")))
  trainInds <- sample(1:nrow(dataGbm), floor(nrow(dataGbm)*0.9))
  tmpTrain <- dataGbm[trainInds,]
  tmpTest <- dataGbm[-trainInds,]
  if(covarType == "Full"){
    gbmFormula <- as.formula(paste(resp,"~",paste(allCovar, collapse="+")))
  }else{
    gbmFormula <- as.formula(paste(resp,"~",paste(nationalCovar, collapse="+")))
  }
  wts <- 1/dataGbm[,gsub("_Estimate","_StdError",resp)]^2
  tmpGbm <- gbm(formula = gbmFormula, 
                distribution = "gaussian",
                data = tmpTrain,
                weights = wts[trainInds],
                n.trees = nTrees,
                n.minobsinnode = 2,
                interaction.depth = 2,
                shrinkage = bestPars$shr,
                bag.fraction = bestPars$bf,  
                cv.folds = 10)
  optTrees <- gbm.perf(tmpGbm)
  
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
  
  ## Partial dependence plots
  covs <- switch(covarType,
                 Full = allCovar, Nat = nationalCovar) # see evalGBM.R for objects
  numPlots <- length(covs)
  numP1 <- ceiling(numPlots/2)
  pl <- list()
  for(i in 1:numPlots){
    # i = 1
    # 
    p <- partial(tmpGbm, pred.var = covs[i], train = tmpTrain, n.trees = optTrees)
    f <- plotPartial(p, smooth = TRUE, lwd = 2)
    pl[[i]] <- f
  }
  l1 <- pl[1:numP1]
  p1 <- gridExtra::marrangeGrob(l1, nrow = 4, ncol = 2)
  l2 <- pl[(numP1+1):numPlots]
  p2 <- gridExtra::marrangeGrob(l2, nrow = 4, ncol = 2)
  return(list("gbm"=tmpGbm,"optTrees"=optTrees, "mse" = mse, 
              "rsq_is" = rsq_is,
              "predPlot"=predPlot, "RelInfPlot"=relInfPlot,
              "PDPlots"=list(p1,p2)))
}
