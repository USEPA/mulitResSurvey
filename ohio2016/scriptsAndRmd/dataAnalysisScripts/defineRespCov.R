# SCRIPT FOR DEFINING IMPORTANT VARIABLES USED IN GBM RUNS AND aggregateActon.R


# RESPONSE VARIABLES--------
# List of response variables
respList <- c("ebMlHrM2_Estimate",
              "ch4.trate.mg.h_Estimate",
              "ch4.drate.mg.m2.h_Estimate",
              "ch4.erate.mg.h_Estimate",
              "co2.trate.mg.h_Estimate",
              "co2.drate.mg.m2.h_Estimate",
              "co2.erate.mg.h_Estimate")


# PREDICTOR VARIABLES-------------
# List of covariates to include when aggregating Acton data.  Same
# as allCovar below, but max.depth.ft used here, rather than max.depth.m
# Excluding SI from covarList due to strong correlation with res size
allCovarActon <- c("chla_Estimate", "tp_Estimate", "tn_Estimate", "max.depth.ft",
               "mean.depth.m", "prop.less.3m", "hypoxic.frac", "hypol.frac",
               "res.perimeter.m", "res.fetch.m", "reservoir.area.m2", 
               "watershed.area.m2", "percent.agg.ag", "rda")



# allCovar similar to allCovarActon used in aggregateActon.R (see above), but 
# here max.depth.ft is replaced with max.depth.m
allCovar <- c("chla_Estimate", "tp_Estimate", "tn_Estimate", "max.depth.m",
              "mean.depth.m", "prop.less.3m", "hypoxic.frac", "hypol.frac",
              "res.perimeter.m", "res.fetch.m", "reservoir.area.m2", 
              "watershed.area.m2", "percent.agg.ag", "rda")


# See aggregateActon.R for list.
nationalCovar <- c("max.depth.m", # exclude SI due to correlation with res size
                   "mean.depth.m", "prop.less.3m",
                   "res.perimeter.m", "res.fetch.m", "reservoir.area.m2", 
                   "watershed.area.m2", "percent.agg.ag", "rda")

# DATA---------------------------------
# Using lake-scale estimates of mean and variance calculated from
# spSurvey function (grtsMeanVariance.R).  Replicate observations
# at Acton lake aggregated into one value (aggregateActon.R).

natDataGbm <- meanVariance.c.lake.lu.agg # local + lit data
localDataGbmIndex <- grepl(pattern = "EPA", x = meanVariance.c.lake.lu.agg$citation)
localDataGbm <- meanVariance.c.lake.lu.agg[localDataGbmIndex, ] # local data



