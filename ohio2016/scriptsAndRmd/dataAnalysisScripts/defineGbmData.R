# DEFINE DATA FOR GBM MODELS

# DATA---------------------------------
# Using lake-scale estimates of mean and variance calculated from
# spSurvey function (grtsMeanVariance.R).  Replicate observations
# at Acton lake aggregated into one value (aggregateActon.R).

natDataGbm <- meanVariance.c.lake.lu.agg # local + lit data
localDataGbmIndex <- grepl(pattern = "EPA", x = meanVariance.c.lake.lu.agg$citation)
localDataGbm <- meanVariance.c.lake.lu.agg[localDataGbmIndex, ] # local data