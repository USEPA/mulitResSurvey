## Function that takes a response, a set of covariates, and a National/Full
## designation - and finds the best parameters from previous simulations
runGBM <- function(resp, covarType = "Full", seed = 2222){
  ## This script assumes that the dataGbm object, covarList, and nationalCovar
  ## objects are in the global environment.
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
  bestPars <- evalGbmResults$parameterGrid[orderOSMSE[1],]
  
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
  return(list("gbm"=tmpGbm,"optTrees"=optTrees))
}
