
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

# Add LU and morophology data to grts estimates
source("ohio2016/scriptsAndRmd/descRes.R") # creates meanVariance.c.lake.lu
source("ohio2016/scriptAndRmd/harrisonData.R")  # merges Harrison data w/ res survey
# source(ohio2016/scriptAndRmd/bevelhimerData.R")  # merges Bevelhimer data w/ res survey
source("ohio2016/scriptsAndRmd/aggregateActon.R")  # Aggregate Acton reps.  meanVariance.c.lake.lu.agg

# Data analysis
# load('ohio2016/output/meanVariance.c.lake.lu.RData') # can run obove, or load data
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/exploratoryPlots.R") # linear models
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/statModels.R") # under development
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/responseSurfacePlots.R") # uses models from above
# source("ohio2016/scriptsAndRmd/dataAnalysisScripts/brtModel.R") # 5 hours per model, careful!