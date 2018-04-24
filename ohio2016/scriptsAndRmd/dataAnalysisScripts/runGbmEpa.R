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
  
  tmpGbmRi <- relative.influence(tmpGbm) %>% 
    data.frame(influence = as.numeric(.),
               names = attributes(.)) %>%
    select(influence, names) %>%
    rename(variable = names)
  
  orderList <- order(tmpGbmRi[, "influence"])
  influenceRank <- tmpGbmRi[orderList, "variable"]  
  tmpGbmRi[, "variable"] = factor(tmpGbmRi$variable, 
                                  levels = influenceRank)
  
  riPlot <- ggplot(tmpGbmRi, aes(influence, variable)) +
    geom_point() +
    ggtitle(rdNm)
  
  return(list("gbm"=tmpGbm,"optTrees"=optTrees, "plot" = riPlot))
}

#  APPLY FUNCTION
# Use loop to apply to all combinations of response and predictors
# evalCh4erateNat not written to disk.  WTF!  Gotta fix that.
resp <- c("ch4.trate.mg.h_Estimate", "ch4.drate.mg.m2.h_Estimate", "ch4.erate.mg.h_Estimate",
         "co2.trate.mg.h_Estimate", "co2.drate.mg.m2.h_Estimate", "co2.erate.mg.h_Estimate")
covList = c("Full", "Nat")
respCovList <- expand.grid(resp, covList) %>%
  rename(cov = Var2, resp = Var1) 

respCovList <- respCovList[-9,] ## evalCh4erateNat not written to disk.  WTF!  Gotta fix that.

gbmMod <- list()
Sys.time()
for (i in 1:length(respCovList$resp)) {
  
  gbmMod[[i]] <- runGBM(resp = as.character(respCovList[i, "resp"]), 
                      covarType = as.character(respCovList[i, "cov"]))
}
Sys.time()

# 5, co2.drate.mg.m2.h_Estimate Full not converging.  optTrees = 8903 in eval run,
# 8903 in optimized run.
# 9, co2.trate.mg.h_Estimate National not converging.  optTrees = 7992.4 in eval run,
# 10000 in optimized run.