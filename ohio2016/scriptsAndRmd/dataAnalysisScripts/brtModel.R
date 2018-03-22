# INVESTIGATING BOOSTED REGRESSION TREE FOR TOTAL CH4 EMISSION RATE
# Function 'evalGBM was originated by Neptune and sourced from masterLibrary.R
# Function returns model fit under different shrinkage and bag rates

# Data
# Using lake-scale estimates of mean and variance calculated from
# spSurvey function (grtsMeanVariance.R).  Replicate observations
# at Acton lake aggregated into one value (aggregateActon.R)
dataGbm <- meanVariance.c.lake.lu.agg

# Predictor variables
allCovar = covarList # See aggregateActon.R for list.
nationalCovar <- c("max.depth.ft",
                   "mean.depth.m.morpho", "prop.less.3m",
                   "res.perimeter.m", "res.fetch.m", "reservoir.area.m2", 
                   "watershed.area.m2", "percent.agg.ag", "rda", "si")

# Volumetric ebullition rate---------------------------------
resp = "ebMlHrM2_Estimate"
weights = 1/dataGbm$ebMlHrM2_StdError^2 
nTrees = 10000

# All predictors
evalVolRateFull <- evalGBM(x = dataGbm, 
                        resp = resp,
                        covar = allCovar,
                        weights = weights,
                        nTrees = nTrees)

# Write to disk
save(evalVolRateFull, file = "ohio2016/output/evalVolRateFull.RData")

# Look at plots
evalVolRateFull$plots[[1]]
evalVolRateFull$plots[[2]]
evalVolRateFull$parameterGrid


# National scale predictors
evalVolRateNat <- evalGBM(x = dataGbm, # 11 hours
                           resp = resp,
                           covar = nationalCovar,
                           weights = weights,
                           nTrees = nTrees)


# Write to disk
save(evalVolRateNat, file = "ohio2016/output/evalVolRateNat.RData")

# Look at plots
evalVolRateNat$plots[[1]]
evalVolRateNat$plots[[2]]
evalVolRateNat$parameterGrid

# Total CH4 emission rate---------------------------------
resp = "ch4.trate.mg.h_Estimate"
weights = 1/dataGbm$ch4.trate.mg.h_StdError^2 
nTrees = 10000

# All predictors
evalCh4trateFull <- evalGBM(x = dataGbm, # ~ 5 hours to run
                        resp = resp,
                        covar = allCovar,
                        weights = weights,
                        nTrees = nTrees)

# Write to disk
save(evalCh4trateFull, file = "ohio2016/output/evalCh4trateFull.RData")

# Look at plots
evalCh4trateFull$plots[[1]]
evalCh4trateFull$plots[[2]]
evalCh4trateFull$parameterGrid

# National scale predictors
evalCh4trateNat <- evalGBM(x = dataGbm, 
                            resp = resp,
                            covar = nationalCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalCh4trateNat, file = "ohio2016/output/evalCh4trateNat.RData")

# Look at plots
evalch4trateNat$plots[[1]]
evalCh4trateNat$plots[[2]]
evalCh4trateNat$parameterGrid

# Diffusive CH4 emission rate---------------------------------
resp = "ch4.drate.mg.m2.h_Estimate"
weights = 1/dataGbm$ch4.drate.mg.m2.h_StdError^2 
nTrees = 10000

# All predictors
evalCh4drateFull <- evalGBM(x = dataGbm, 
                            resp = resp,
                            covar = allCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalCh4drateFull, file = "ohio2016/output/evalCh4drateFull.RData")

# Look at plots
evalCh4drateFull$plots[[1]]
evalCh4drateFull$plots[[2]]
evalCh4drateFull$parameterGrid

# National scale predictors
evalCh4drateNat <- evalGBM(x = dataGbm, 
                            resp = resp,
                            covar = nationalCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalCh4drateNat, file = "ohio2016/output/evalCh4drateNat.RData")

# Look at plots
evalCh4drateNat$plots[[1]]
evalCh4drateNat$plots[[2]]
evalCh4drateNat$parameterGrid

# Ebullitive CH4 emission rate---------------------------------
resp = "ch4.erate.mg.h_Estimate"
weights = 1/dataGbm$ch4.erate.mg.h_StdError^2 
nTrees = 10000

# All predictors
evalCh4erateFull <- evalGBM(x = dataGbm, 
                            resp = resp,
                            covar = allCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalCh4erateFull, file = "ohio2016/output/evalCh4erateFull.RData")

# Look at plots
evalCh4erateFull$plots[[1]]
evalCh4erateFull$plots[[2]]
evalCh4erateFull$parameterGrid

# National scale predictors
evalCh4erateNat <- evalGBM(x = dataGbm, 
                           resp = resp,
                           covar = nationalCovar,
                           weights = weights,
                           nTrees = nTrees)

# Write to disk
save(evalCh4erateNat, file = "ohio2016/output/evalCh4erateNat.RData")

# Look at plots
evalCh4erateNat$plots[[1]]
evalCh4erateNat$plots[[2]]
evalCh4erateNat$parameterGrid

# CO2 total emission rate---------------------------------
resp = "co2.trate.mg.h_Estimate"
weights = 1/dataGbm$co2.trate.mg.h_StdError^2 
nTrees = 10000

# All predictors
evalCo2trateFull <- evalGBM(x = dataGbm, 
                            resp = resp,
                            covar = allCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalCo2trateFull, file = "ohio2016/output/evalCo2trateFull.RData")

# Look at plots
evalCo2trateFull$plots[[1]]
evalCo2trateFull$plots[[2]]
evalCo2trateFull$parameterGrid

# National scale predictors
evalCo2trateNat <- evalGBM(x = dataGbm, 
                           resp = resp,
                           covar = nationalCovar,
                           weights = weights,
                           nTrees = nTrees)

# Write to disk
save(evalCo2trateNat, file = "ohio2016/output/evalCo2trateNat.RData")

# Look at plots
evalCo2trateNat$plots[[1]]
evalCo2trateNat$plots[[2]]
evalCo2trateNat$parameterGrid

# CO2 diffusive emission rate---------------------------------
resp = "co2.drate.mg.m2.h_Estimate"
weights = 1/dataGbm$co2.drate.mg.m2.h_StdError^2 
nTrees = 10000

# All predictors
evalCo2drateFull <- evalGBM(x = dataGbm, 
                            resp = resp,
                            covar = allCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalCo2drateFull, file = "ohio2016/output/evalCo2drateFull.RData")

# Look at plots
evalCo2drateFull$plots[[1]]
evalCo2drateFull$plots[[2]]
evalCo2drateFull$parameterGrid

# National scale predictors
evalCo2drateNat <- evalGBM(x = dataGbm, 
                           resp = resp,
                           covar = nationalCovar,
                           weights = weights,
                           nTrees = nTrees)

# Write to disk
save(evalCo2drateNat, file = "ohio2016/output/evalCo2drateNat.RData")

# Look at plots
evalCo2drateNat$plots[[1]]
evalCo2drateNat$plots[[2]]
evalCo2drateNat$parameterGrid

# CO2 ebullitive emission rate---------------------------------
resp = "co2.erate.mg.h_Estimate"
weights = 1/dataGbm$co2.erate.mg.h_StdError^2 
nTrees = 10000

# All predictors
evalCo2erateFull <- evalGBM(x = dataGbm, 
                            resp = resp,
                            covar = allCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalCo2erateFull, file = "ohio2016/output/evalCo2erateFull.RData")

# Look at plots
evalCo2erateFull$plots[[1]]
evalCo2erateFull$plots[[2]]
evalCo2erateFull$parameterGrid

# National scale predictors
evalCo2erateNat <- evalGBM(x = dataGbm.t, 
                           resp = resp,
                           covar = nationalCovar,
                           weights = weights,
                           nTrees = nTrees)

# Write to disk
save(evalCo2erateNat, file = "ohio2016/output/evalCo2erateNat.RData")

# Look at plots
evalCo2erateNat$plots[[1]]
evalCo2erateNat$plots[[2]]
evalCo2erateNat$parameterGrid