

# Need to follow up on 16242.  readGcdata

# Check on Cave Run water chem sampling sites.  Listed as SU-07 and SU-50
# in eqAreaData.  However, chem data indicate SU-04, SU-07, and SU-46; however
# SU-46 and SU-04 seem to be from same site.

# Missing chem data from:
# Acton Lake (ACN, 2016-05-31), site ids U04 and U18 in chem file for this
# sampling date.  I think this is correct, double check at office.

# Lake Loramie (LOR, 2016-08-03), chem file contains LR and LRN from 08-02?
# Not sure, will wait until I get to office to confirm.

# Change TNO2-3 name.  It fucks with grtsMeanVariance function.  Eliminate
# dash.


# calculateEmissions.R
# Need to strip out diffusion fits with an r2 < xx.
# Set CH4 diffusion for Brookville SU-11 to NA.  Model fit to a portion
# of data after ebullition is way to big.