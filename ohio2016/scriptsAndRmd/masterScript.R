
# Load libraries and functions
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# Read raw data
source("ohio2016/scriptsAndRmd/readSitesEqAreaData.R") # Reads shapefiles, 30s
source("ohio2016/scriptsAndRmd/readGc.R") # GC data, merges with eqAreaData, 15s
source("ohio2016/scriptsAndRmd/readLgr.R") # Reads in raw LGR data, merge w/CowanGC, 16s
source("ohio2016/scriptsAndRmd/readChem.R") # Merges with eqAreaData
source("ohio2016/scriptsAndRmd/readChl.R") # Merges with eqAreaData

# Calculate derived quantities
source("ohio2016/scriptsAndRmd/calculateDissGas.R") # Merges with eqAreaData
source("ohio2016/scriptsAndRmd/plotCleanLgr.R") # Merges chamber time with eqAreaData, 10min
source("ohio2016/scriptsAndRmd/calculateEmissions.R") # Merges with eqAreaData, 3min


# grts calculations
source("ohio2016/scriptsAndRmd/grtsWgtAdj.R") # Merges with eqAreaData, 2s
source("ohio2016/scriptsAndRmd/grtsMeanVariance.R") # 20s

# Add LU and morophology data to grts estimates. Prep for analysis.
source("ohio2016/scriptsAndRmd/descRes.R") # creates meanVariance.c.lake.lu
source("ohio2016/scriptsAndRmd/harrisonData.R")  # merges Harrison data w/ meanVariance.c.lake.lu
source("ohio2016/scriptsAndRmd/bevelhimerData.R")  # merges Bevelhimer data w/ meanVariance.c.lake.lu
source("ohio2016/scriptsAndRmd/readNHD.R") # read NHD data (i.e lakeMorpho, lakeCat, streamCat)
source("ohio2016/scriptsAndRmd/calculateMorpho.R")  # calculates derived quantities
source("ohio2016/scriptsAndRmd/aggregateActon.R")  # Aggregate Acton reps.  meanVariance.c.lake.lu.agg
source("ohio2016/scriptsAndRmd/defineRespCov.R") # OneDrive, for now.  response and covarlists for gbm

# Exploratory Data Analysis and Linear Models
# load('ohio2016/output/meanVariance.c.lake.lu.RData') # can run obove, or load data
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/linearModels/exploratoryPlots.R") # linear models
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/linearModels/statModels.R") # under development
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/responseSurfacePlots.R") # uses models from above

# GBM scripts

# evalGBM.R calls evalGBM() which is sourced from masterLibrary.R
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/defineGbmData.R") # Define local and national for model
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/evalGBM.R") # 5 hours per model, careful!
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/evalGBMresults.Rmd") # contour plots for evalGBM() generated objects
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/evalGBMcompare.R") # Effect of nGBM and seed on reproducability
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/evalKmeansTrainProp.R") # Effect of trainProp and cvFolds on reproducability

# Function for running gbm using ideal parameters extracted from
# object created in evalGBM.R.  First version of function written
# by Will.
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/runGBM.V.1.R") # function for runGbm function, produce gbmohio.pdf

# Function for running gbm using ideal parameters extracted from
# object created in evalGBM.R.  This version was written by Beaulieu
# and uses parameters extracted from evalGBM using nGBM > 10.
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/runGBM.v2.R")

# Function for running gbm using reasonable values for bf and shr.  Does
# not depend on evalGBM.R
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/runGBM.v3.R")

# Function for running gbm using reasonable values for bf and shr.  Use kmeans
# for choosing representative training data.  Does not depend on evalGBM.R
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/runGBM.v4.R")

# Explore variation in relative importance plots across 'replicate' gbm runs.
# This executes runGBM.v4 and produces 850 gbm models.  Must be run on VM.
# From these runs we extracted 1 model for each resp ~ cov
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/evalVIPlots.R) 

# Explore final gbm models for each set of observations, response, and 
# covariates.
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/inspectFinalGbmModels.R") # covariates do not include NHD data
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/inspectFinalGbmModelsNHD.R") # includes NHD covariates
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/inspectFinalGbmModelsNHDbest.R") # best models w/NHD covariates