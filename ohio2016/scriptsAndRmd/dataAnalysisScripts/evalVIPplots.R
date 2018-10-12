## PREVIOUS SIMULATIONS WERE USED TO IDENTIFY ACCEPTABLE VALUES FOR BF, SHR,
## CV FOLDS, AND TRAIN PROPORTION.  SIMULATIONS ALSO SHOWED THAT osMSE VALUES
## WERE APPROXIMATELY LOG-NORMALLY DISTRIBUTED ACROSS REPEAT RUNS USING ARGUMENT
## VALUES REFERENCED ABOVE.  OUR PROPOSAL IS TO ONLY ACCEPT GBMs THAT HAVE > 1000
## TREES, < maxTREES, AND AN osMSE LOWER THAN THE 75TH PERCENTILE OF osMSE
## VALUES OBSERVED ACROSS 50 REPLICATE RUNS.  THIS APPROACH COULD BE USED TO SELECT
## A SINGLE gbm FOR EACH RESPONSE ~ COVARIATE MODEL.  THERE IS SOME CONCERN, HOWEVER,
## THAT THIS SINGLE MODEL MAY NOT BE 'REPRESENTATIVE' OF ALL THE POTENTIAL MODELS THAT
## MEET THE CRITERIA DESCRIBED ABOVE.  IN THE FOLLOWING CODE WE WILL GENERATE 50
## 'REPLICATE' MODELS USING THE CRITERIA ABOVE.  WE WILL THEN EXAMING THE VARIABLE
## IMPORTANCE RANKINGS TO SEE IF THEY ARE LARGELY CONSISTENT, OR HUGELY VARIABLE.


# evalKmeansTrainProp.R was used to generate 50 'replicate' models for tCH4.
# We will use the 75th percentile of the osMSE distribution as an acceptance
# here.  
osMseCriteria <- evalGBMKmeansResults %>% # see evalKmeansTrainProp.R
  filter(cvFolds == 10, trainProp == 0.9) %>% # bf=0.9, shr=0.0005 for all runs
  summarize(criteria = quantile(osMSE, 0.75)) %>% #75th percentile
  pull(criteria) # convert to named vector

cList <- list()

for(i in 1:5) {
  message(i)
  repeat{
    message(paste0("failed",i))
    tmpGBM <- runGBM.v4(resp = "ch4.trate.mg.h_Estimate", covarType = "Full", 
                        obs = "Local", nTrees = 20000,
                        seed = 2222+i, bf = 0.9, shr = 0.0005, trainProp = 0.9)
    osMSE <- tmpGBM$mse[2,2]
    if(osMSE < osMseCriteria){
      break
    }
  }
  cList[[i]] <- tmpGBM
}







poo <- 
lapply(cList, function(x) {
  x$gbm %>% 
    summary() %>% 
    select(var) %>%
    t() %>%
    as.data.frame() %>%
    mutate_all(as.character) %>%
    setNames(1:ncol(.)) 
    }) %>%
  do.call(rbind, .)

table(poo$'1')


