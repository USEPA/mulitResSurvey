# INVESTIGATING BOOSTED REGRESSION TREE
# Function 'evalGBM was originated by Neptune and sourced from masterLibrary.R
# Function returns model fit under different shrinkage and bag rates

# Data
# Using lake-scale estimates of mean and variance calculated from
# spSurvey function (grtsMeanVariance.R).  Replicate observations
# at Acton lake aggregated into one value (aggregateActon.R).
# Dependent variables include volumtric ebullition, CH4 emisson (3 mechanisms),
# and CO2 emissions (3 mechanisms).  Predictor variables include those found
# in national data sets, and national + local data (i.e., chlorophyll).
# Function run for only local measurements AND local + lit data.

natDataGbm <- meanVariance.c.lake.lu.agg # local + lit data
localDataGbmIndex <- grepl(pattern = "EPA", x = meanVariance.c.lake.lu.agg$citation)
localDataGbm <- meanVariance.c.lake.lu.agg[localDataGbmIndex, ] # local data

# Predictor variables
allCovar = covarList # See aggregateActon.R for list.
nationalCovar <- c("max.depth.m", # exclude SI due to correlation with res size
                   "mean.depth.m.morpho", "prop.less.3m",
                   "res.perimeter.m", "res.fetch.m", "reservoir.area.m2", 
                   "watershed.area.m2", "percent.agg.ag", "rda")

# Volumetric ebullition rate---------------------------------
resp = "ebMlHrM2_Estimate"
weights = 1/dataGbm$ebMlHrM2_StdError^2 
nTrees = 10000

# All predictors: local emission data 
evalLocalVolRateFull <- evalGBM(x = localDataGbm, 
                        resp = resp,
                        covar = allCovar,
                        weights = weights,
                        nTrees = nTrees)

# Write to disk
save(evalLocalVolRateFull, file = "ohio2016/output/evalLocalVolRateFull.RData")

# Look at plots
evalLocalVolRateFull$plots[[1]]
evalLocalVolRateFull$plots[[2]]
evalLocalVolRateFull$parameterGrid


# National scale predictors : local emission data
evalLocalVolRateNat <- evalGBM(x = localDataGbm, # 11 hours
                           resp = resp,
                           covar = nationalCovar,
                           weights = weights,
                           nTrees = nTrees)


# Write to disk
save(evalLocalVolRateNat, file = "ohio2016/output/evalLocalVolRateNat.RData")

# Look at plots
evalLocalVolRateNat$plots[[1]]
evalLocalVolRateNat$plots[[2]]
evalLocalVolRateNat$parameterGrid

# National scale predictors: National scale emission data
evalNatVolRateNat <- evalGBM(x = natDataGbm, # 11 hours
                               resp = resp,
                               covar = nationalCovar,
                               weights = weights,
                               nTrees = nTrees)


# Write to disk
save(evalNatVolRateNat, file = "ohio2016/output/evalNatVolRateNat.RData")

# Look at plots
evalNatVolRateNat$plots[[1]]
evalNatVolRateNat$plots[[2]]
evalNatVolRateNat$parameterGrid

# Total CH4 emission rate---------------------------------
resp = "ch4.trate.mg.h_Estimate"
weights = 1/dataGbm$ch4.trate.mg.h_StdError^2 
nTrees = 10000

# All predictors: local emission data
evalLocalCh4trateFull <- evalGBM(x = localDataGbm, # ~ 5 hours to run
                        resp = resp,
                        covar = allCovar,
                        weights = weights,
                        nTrees = nTrees)

# Write to disk
save(evalLocalCh4trateFull, file = "ohio2016/output/evalLocalCh4trateFull.RData")

# Look at plots
evalLocalCh4trateFull$plots[[1]]
evalLocalCh4trateFull$plots[[2]]
evalLocalCh4trateFull$parameterGrid

# National scale predictors : local emission data
evalLocalCh4trateNat <- evalGBM(x = localDataGbm, 
                            resp = resp,
                            covar = nationalCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalLocalCh4trateNat, file = "ohio2016/output/evalLocalCh4trateNat.RData")

# Look at plots
evalLocalCh4trateNat$plots[[1]]
evalLocalCh4trateNat$plots[[2]]
evalLocalCh4trateNat$parameterGrid

# National scale predictors : national emission data
evalNatCh4trateNat <- evalGBM(x = natDataGbm, 
                                resp = resp,
                                covar = nationalCovar,
                                weights = weights,
                                nTrees = nTrees)

# Write to disk
save(evalNatCh4trateNat, file = "ohio2016/output/evalNatCh4trateNat.RData")

# Look at plots
evalNatCh4trateNat$plots[[1]]
evalNatCh4trateNat$plots[[2]]
evalNatCh4trateNat$parameterGrid

# Diffusive CH4 emission rate---------------------------------
resp = "ch4.drate.mg.m2.h_Estimate"
weights = 1/dataGbm$ch4.drate.mg.m2.h_StdError^2 
nTrees = 10000

# All predictors : local emission data
evalLocalCh4drateFull <- evalGBM(x = localDataGbm, 
                            resp = resp,
                            covar = allCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalLocalCh4drateFull, file = "ohio2016/output/evalLocalCh4drateFull.RData")

# Look at plots
evalLocalCh4drateFull$plots[[1]]
evalLocalCh4drateFull$plots[[2]]
evalLocalCh4drateFull$parameterGrid

# National scale predictors : local scale emission data
evalLocalCh4drateNat <- evalGBM(x = localDataGbm, 
                            resp = resp,
                            covar = nationalCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalLocalCh4drateNat, file = "ohio2016/output/evalLocalCh4drateNat.RData")

# Look at plots
evalLocalCh4drateNat$plots[[1]]
evalLocalCh4drateNat$plots[[2]]
evalLocalCh4drateNat$parameterGrid

# National scale predictors : national scale emission data
evalNatCh4drateNat <- evalGBM(x = natDataGbm, 
                                resp = resp,
                                covar = nationalCovar,
                                weights = weights,
                                nTrees = nTrees)

# Write to disk
save(evalNatCh4drateNat, file = "ohio2016/output/evalNatCh4drateNat.RData")

# Look at plots
evalNatCh4drateNat$plots[[1]]
evalNatCh4drateNat$plots[[2]]
evalNatCh4drateNat$parameterGrid

# Ebullitive CH4 emission rate---------------------------------
resp = "ch4.erate.mg.h_Estimate"
weights = 1/dataGbm$ch4.erate.mg.h_StdError^2 
nTrees = 10000

# All predictors : local emission data
evalLocalCh4erateFull <- evalGBM(x = localDataGbm, 
                            resp = resp,
                            covar = allCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalLocalCh4erateFull, file = "ohio2016/output/evalLocalCh4erateFull.RData")

# Look at plots
evalLocalCh4erateFull$plots[[1]]
evalLocalCh4erateFull$plots[[2]]
evalLocalCh4erateFull$parameterGrid

# National scale predictors : local emission data
evalLocalCh4erateNat <- evalGBM(x = localDataGbm, 
                           resp = resp,
                           covar = nationalCovar,
                           weights = weights,
                           nTrees = nTrees)

# Write to disk
save(evalLocalCh4erateNat, file = "ohio2016/output/evalLocalCh4erateNat.RData")

# Look at plots
evalLocalCh4erateNat$plots[[1]]
evalLocalCh4erateNat$plots[[2]]
evalLocalCh4erateNat$parameterGrid

# National scale predictors : national emission data
evalLocalCh4erateNat <- evalGBM(x = natDataGbm, 
                                resp = resp,
                                covar = nationalCovar,
                                weights = weights,
                                nTrees = nTrees)

# Write to disk
save(evalNatCh4erateNat, file = "ohio2016/output/evalNatCh4erateNat.RData")

# Look at plots
evalNatCh4erateNat$plots[[1]]
evalNatCh4erateNat$plots[[2]]
evalNatCh4erateNat$parameterGrid

# CO2 total emission rate---------------------------------
resp = "co2.trate.mg.h_Estimate"
weights = 1/dataGbm$co2.trate.mg.h_StdError^2 
nTrees = 10000

# All predictors : local emission data
evalLocalCo2trateFull <- evalGBM(x = localDataGbm, 
                            resp = resp,
                            covar = allCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalLocalCo2trateFull, file = "ohio2016/output/evalLocalCo2trateFull.RData")

# Look at plots
evalLocalCo2trateFull$plots[[1]]
evalLocalCo2trateFull$plots[[2]]
evalLocalCo2trateFull$parameterGrid

# National scale predictors : local emission data
evalLocalCo2trateNat <- evalGBM(x = localDataGbm, 
                           resp = resp,
                           covar = nationalCovar,
                           weights = weights,
                           nTrees = nTrees)

# Write to disk
save(evalLocalCo2trateNat, file = "ohio2016/output/evalLocalCo2trateNat.RData")

# Look at plots
evalLocalCo2trateNat$plots[[1]]
evalLocalCo2trateNat$plots[[2]]
evalLocalCo2trateNat$parameterGrid

# National scale predictors : national emission data
evalNatCo2trateNat <- evalGBM(x = natDataGbm, 
                                resp = resp,
                                covar = nationalCovar,
                                weights = weights,
                                nTrees = nTrees)

# Write to disk
save(evalNatCo2trateNat, file = "ohio2016/output/evalNatCo2trateNat.RData")

# Look at plots
evalNatCo2trateNat$plots[[1]]
evalNatCo2trateNat$plots[[2]]
evalNatCo2trateNat$parameterGrid
# CO2 diffusive emission rate---------------------------------
resp = "co2.drate.mg.m2.h_Estimate"
weights = 1/dataGbm$co2.drate.mg.m2.h_StdError^2 
nTrees = 10000

# All predictors : local emission data
evalLocalCo2drateFull <- evalGBM(x = localDataGbm, 
                            resp = resp,
                            covar = allCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalLocalCo2drateFull, file = "ohio2016/output/evalLocalCo2drateFull.RData")

# Look at plots
evalLocalCo2drateFull$plots[[1]]
evalLocalCo2drateFull$plots[[2]]
evalLocalCo2drateFull$parameterGrid

# National scale predictors : local emission data
evalLocalCo2drateNat <- evalGBM(x = localDataGbm, 
                           resp = resp,
                           covar = nationalCovar,
                           weights = weights,
                           nTrees = nTrees)

# Write to disk
save(evalLocalCo2drateNat, file = "ohio2016/output/evalLocalCo2drateNat.RData")

# Look at plots
evalLocalCo2drateNat$plots[[1]]
evalLocalCo2drateNat$plots[[2]]
evalLocalCo2drateNat$parameterGrid

# National scale predictors : national emission data
evalNatCo2drateNat <- evalGBM(x = natDataGbm, 
                                resp = resp,
                                covar = nationalCovar,
                                weights = weights,
                                nTrees = nTrees)

# Write to disk
save(evalNatCo2drateNat, file = "ohio2016/output/evalNatCo2drateNat.RData")

# Look at plots
evalNatCo2drateNat$plots[[1]]
evalNatCo2drateNat$plots[[2]]
evalNatCo2drateNat$parameterGrid
# CO2 ebullitive emission rate---------------------------------
resp = "co2.erate.mg.h_Estimate"
weights = 1/dataGbm$co2.erate.mg.h_StdError^2 
nTrees = 10000

# All predictors : local emission data
evalLocalCo2erateFull <- evalGBM(x = localDataGbm, 
                            resp = resp,
                            covar = allCovar,
                            weights = weights,
                            nTrees = nTrees)

# Write to disk
save(evalLocalCo2erateFull, file = "ohio2016/output/evalLocalCo2erateFull.RData")

# Look at plots
evalLocalCo2erateFull$plots[[1]]
evalLocalCo2erateFull$plots[[2]]
evalLocalCo2erateFull$parameterGrid

# National scale predictors : local emission data
evalLocalCo2erateNat <- evalGBM(x = localDataGbm, 
                           resp = resp,
                           covar = nationalCovar,
                           weights = weights,
                           nTrees = nTrees)

# Write to disk
save(evalLocalCo2erateNat, file = "ohio2016/output/evalLocalCo2erateNat.RData")

# Look at plots
evalLocalCo2erateNat$plots[[1]]
evalLocalCo2erateNat$plots[[2]]
evalLocalCo2erateNat$parameterGrid

# National scale predictors : national emission data
evalNatCo2erateNat <- evalGBM(x = natDataGbm, 
                                resp = resp,
                                covar = nationalCovar,
                                weights = weights,
                                nTrees = nTrees)

# Write to disk
save(evalNatCo2erateNat, file = "ohio2016/output/evalNatCo2erateNat.RData")

# Look at plots
evalNatCo2erateNat$plots[[1]]
evalNatCo2erateNat$plots[[2]]
evalNatCo2erateNat$parameterGrid