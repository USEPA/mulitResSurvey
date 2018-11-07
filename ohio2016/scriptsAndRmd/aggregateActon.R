# Code for aggregating replicate Acton Lake observation
# into a single estimate of central tendency and variance.
# Adadpted from code supplied by Neptune, Feb. 2018

## Acton Lake exists multiple times. Aggregate the entries in a 
## reasonable way.
## See defineRespCov.R for list of variables to be aggregated.
actonInds <- grepl("Acton", meanVariance.c.lake.lu$Lake_Name)
actonDat <- meanVariance.c.lake.lu[actonInds,
                                   c(respList, 
                                     gsub("Estimate","StdError",respList),
                                               allCovarActon)
                                   ]
outVec <- NULL
for(i in 1:ncol(actonDat)){
  # i = 8
  # Estimates can be averaged.
  tmpNm <- names(actonDat)[i]
  if(!grepl("StdError", tmpNm)){
    aggEst <- mean(actonDat[,i], na.rm = TRUE)
  } else {
    # Count the number of non-NA SE estimates
    numNA <- sum(is.na(actonDat[,i]))
    # The variance of independent random variables is just the sum of the variances.
    # Since we're taking a mean of random variables here, we're really
    # interested in the variance of the mean of those variables. This is just
    # the sum of the variances * (1 / n)^2
    wt <- 1/(nrow(actonDat) - numNA)
    aggEst <- sqrt(wt^2 * sum(actonDat[,i]^2))
  }
  outVec <- c(outVec, aggEst)
}
tmp <- data.frame(t(outVec))
names(tmp) <- names(actonDat)
tmp$citation <- "EPA" # Add citation column
tmp$Lake_Name <- "Acton Lake" # Add Lake_Name

ncol(meanVariance.c.lake.lu); nrow(meanVariance.c.lake.lu) # 119 columns, 46 rows

meanVariance.c.lake.lu.agg <- rbind(tmp, 
                                    meanVariance.c.lake.lu[!actonInds,
                                                           c(respList, 
                                                             gsub("Estimate","StdError",respList),
                                                             allCovarActon, "citation", "Lake_Name")
                                                           ])

# Columns restricted to response variables, covariates, and id variables
# (i.e. citation, Lake_Name)
ncol(meanVariance.c.lake.lu.agg); nrow(meanVariance.c.lake.lu.agg) # 30 columns, 43 rows
