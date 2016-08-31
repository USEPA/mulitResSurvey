# ANALYSIS OF A GRTS SURVEY DESIGN FOR AN AREA RESOURCE

# Load the spsurvey package
library(spsurvey)

# Load the data set and determine the number of rows in the data frame
data(SC_estuaries)
nr <- nrow(SC_estuaries)

# Display the initial six lines in the data file
head(SC_estuaries)

# Display the final six lines in the data file
tail(SC_estuaries)

# ANALYSIS OF SITE STATUS EVALUATION VARIABLE-----------------------
# Evaluation status indicates whether a site is in the population or not.
# This information can be used to estimate the size of the resource. 


addmargins(table(SC_estuaries$Status)) # look at # of sampled and NonTarget sites

# Create the sites data frame.
sites <- data.frame(siteID=SC_estuaries$siteID,
                       Use=rep(TRUE, nr))  # use sampled AND nonTarget sites

#Create the subpop data frame.
subpop <- data.frame(siteID=SC_estuaries$siteID,
                        All_Estuaries=rep("All Estuaries", nr),
                        Estuary_Type=SC_estuaries$Stratum)

#Create the design data frame.
design <- data.frame(siteID=SC_estuaries$siteID,
                        wgt=SC_estuaries$wgt,
                        xcoord=SC_estuaries$xcoord,
                        ycoord=SC_estuaries$ycoord)

#Create the data.cat data frame.
data.cat <- data.frame(siteID=SC_estuaries$siteID,
                          Status=SC_estuaries$Status)  # categorical variable to be evaluated

# Calculate extent estimates for the site status evaluation variables
Extent_Estimates <- cat.analysis(sites, subpop, design, data.cat)
print(Extent_Estimates)



# ANALYSIS OF ESTUARY CONDITION VARIABLES-----------------
# This isn't relevant to the regional CH4 survey because
# no categorical variables were recorded.

addmargins(table(SC_estuaries$IBI_status))
addmargins(table(SC_estuaries$WQ_status))

sites <- data.frame(siteID=SC_estuaries$siteID,
                    Use=SC_estuaries$Status == "Sampled")  # only using sampled sites!

#  Create the data.cat data frame.
data.cat <- data.frame(siteID=SC_estuaries$siteID,
                       IBI_Status=SC_estuaries$IBI_status,
                       WQ_Status=SC_estuaries$WQ_status)

# Calculate estimates for the categorical variables
Condition_Estimates <- cat.analysis(sites, subpop, design, data.cat)
print(Condition_Estimates[c(1:4, 13:16),])


# ANALYSIS OF ESTUARY CONDITION VARIABLES CORRECTING FOR POPULATION SIZE-------
# This is relevant when the survey includes inaccesible sites.

# Assign frame size values
framesize <- c("Open Water"=628.509298, # These data come from the extent estimates above
               "Tidal Creek"=105.829522)  

# Use the cat.analysis function to calculate estimates for the estuary condition variables.
Condition_Estimates_popsize <- cat.analysis(sites, subpop, design, data.cat,
                                            popsize=list(All_Estuaries=sum(framesize),
                                                         Estuary_Type=as.list(framesize)))
print(Condition_Estimates_popsize[c(1:4, 13:16),]) # compare to result of line 65.

# This correction changes the areal extent estimate (Estimate.U)

# ANALYSIS OF QUANTITATIVE VARIABLES--------------------------
summary(SC_estuaries$IBI_score)
summary(SC_estuaries$WQ_score)

data.cont <- data.frame(siteID=SC_estuaries$siteID,
                        IBI_Score=SC_estuaries$IBI_score, # Continuous variable to be analyzed
                        WQ_Score=SC_estuaries$WQ_score) # Continuous variable to be analyzed

# Use the cont.analysis function to calculate CDF and percentile estimates.
CDF_Estimates <- cont.analysis(sites, subpop, design, data.cont,
                               popsize=list(All_Estuaries=sum(framesize),
                                            Estuary_Type=as.list(framesize)))

# Print the percentile estimates for IBI score for all sites combined
print(CDF_Estimates$Pct[1:10,])















