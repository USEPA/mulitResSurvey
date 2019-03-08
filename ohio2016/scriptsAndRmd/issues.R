#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# DISSOLVED GAS ISSUES
# Brookville Lake missing smDpth for SU-35, which breaks code for filling in
# BrPrssr (readSitesEqAreaData.R, line 106), therefore can't calculate dissolved
# gas concentration.

# Buckhorn Lake missing smDpth for SU-30, which breaks code for filling in
# BrPrssr (readSitesEqAreaData.R, line 106), therefore can't calculate dissolved
# gas concentration.

# Waynoka only has DG_Extn for one station (SU-01).  What about other station? 
# Check on this via ArcMap at AWBERC

# Once these are fixed, add dissolved.ch4_Estimate and dissolved.co2_Estimate
# to defineRespCov.R

# 2/12/2019
# Forgot about above details.  I assumed we didn't have these samples and
# hardcoded a value in grtsMeanVariance.R line 43.  Need to follow up w/above

# 11-19-2018
# I updated GC file (updated2018-03-15.txt) in read.gc.  This file contains
# dissolved gas for Acton Aug and Oct.  Need to re-run scripts to incorporate these
# data into project.
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


# 2/12/2018
# line 167 in readNHD.R breaks.  Need to investigate.






# Need to follow up on 16242.  readGcdata


# Brookeville chem data:
# Sample from SU-35 is included in 2017_ESF_EFWS_NutrientData.... and is recorded
# with site ID SU-35_2.  One of the SU-35 dups was lost.  Also, the TP vial from
# the remaining SU-35 site broke in the freezer or during the autoclave.  No TP 
# measured from this site, so I estimated from an overall TP ~ TRP model.  No TOC
# data from this sample.  I think Karen spilled sample.  Please confirm (4/7/17).


# exploratoryPlots.R
# Revisit how ag land use is aggregrated.
# Revisit Paul delGorgio's talk and put together list of lake characteristics to consider.

