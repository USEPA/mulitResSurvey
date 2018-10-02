## Function that takes a response, a set of covariates, and a National/Full
## designation - and finds the best parameters from previous simulations
runGBM.v1 <- function(resp, covarType = "Full", nTrees = 10000, seed = 2222){
  ## This script assumes that the dataGbm object, covarList, and nationalCovar
  ## objects are in the global environment.
  ## covarType can be "Full" or "Nat"
  # resp = "co2.trate.mg.h_Estimate"; covarType = "Full"; seed = 2222
  
  ## Read in relevant R workspace.
  gasNm <- ifelse(grepl("ch4",resp), "Ch4", "Co2")
  gasSrc <- ifelse(grepl("trate",resp), "trate", 
                   ifelse(grepl("erate", resp), "erate","drate"))
  rdNm <- paste("eval", gasNm, gasSrc, 
                ifelse(covarType == "Full","Full","Nat"),
                ".RData", sep="")
  load(paste("ohio2016/output/", rdNm, sep=""))
  
  ## Assign the evalGbm returned object to a known named object.
  assign("evalGbmResults", eval(parse(text=gsub(".RData","",rdNm))))
  
  ## Look for the best parameter set
  orderOSMSE <- order(evalGbmResults$parameterGrid$osMSE)
  orderOptTrees <- evalGbmResults$parameterGrid$optTrees[orderOSMSE]
  treesMaxInds <- which(abs(orderOptTrees-10000)<10)
  if(length(treesMaxInds)>0) orderOSMSE <- orderOSMSE[-treesMaxInds]
  bestPars <- evalGbmResults$parameterGrid[orderOSMSE[1],] # '1' is location of min OSMSE
  
  ## Run gbm model
  set.seed(seed)
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
    ggtitle(paste(gasNm, gasSrc, "~", covarType, "\n",
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
                 Full = covarList, Nat = nationalCovar) # see evalGBM.R for objects
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
