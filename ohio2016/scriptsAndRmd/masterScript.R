
# Load libraries and functions
source("ohio2016/scriptsAndRmd/masterLibrary.R")

# Read raw data
source("ohio2016/scriptsAndRmd/readSitesEqAreaData.R") # Reads shapefiles
source("ohio2016/scriptsAndRmd/readLgr.R") # Reads in raw LGR data
source("ohio2016/scriptsAndRmd/readGc.R") # GC data, merges with eqAreaData
source("ohio2016/scriptsAndRmd/readChem.R") # Merges with eqAreaData

# Calculate derived quantities
source("ohio2016/scriptsAndRmd/plotCleanLgr.R") # Merges chamber time with eqAreaData
source("ohio2016/scriptsAndRmd/calculateEmissions.R") # Merges with eqAreaData

# grts calculations
source("ohio2016/scriptsAndRmd/grtsWgtAdj.R") # Merges with eqAreaData
source("ohio2016/scriptsAndRmd/grtsMeanVariance.R")

# Data analysis
source("ohio2016/scriptsAndRmd/exploratoryPlots.R")
