
# Load libraries and functions
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# Read raw data
source("ohio2016/scriptsAndRmd/readSitesEqAreaData.R")
source("ohio2016/scriptsAndRmd/readLgr.R")
source("ohio2016/scriptsAndRmd/readGc.R")

# Calculate derived quantities
source("ohio2016/scriptsAndRmd/plotCleanLgr.R")
source("ohio2016/scriptsAndRmd/calculateEmissions.R")

# grts calculations
source("ohio2016/scriptsAndRmd/grtsWgtAdj.R")
source("ohio2016/scriptsAndRmd/grtsMeanVariance.R")

# Data analysis
source("ohio2016/scriptsAndRmd/exploratoryPlots.R")
