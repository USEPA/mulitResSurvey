# UNTRANSFORMED STATISTICAL MODELS
# Basic correlation matrix.
cor(select(meanVariance.c.lake.lu, 
           ebMlHrM2_Estimate,
           ch4.trate.mg.h_Estimate,
           chla_Estimate, #0.19
           tp_Estimate,
           tn_Estimate,
           max.depth.ft,
           res.perimeter.m,
           res.fetch.m, #-0.13
           reservoir.area.m2, #-0.19
           watershed.area.m2,#0.39
           percent.agg.ag, #Ag and TN are 0.8!
           rda, #0.56 
           si), # 0.22
    use = "pairwise.complete.obs")

# Lots of correlations with volumetric, few with mass flux rate


# TOTAL AND VOLUMETRIC EMISSIONS BY LANDSCAPE AND MORPHOMETRIC MODELS----------
# First will work with model containing only morphometric and 
# and watershed indices.
# %ag, depth, reservoir size, watershed size, perimeter, rda, si, fetch
# Lots of collinearity, select non correlated variables based
# on variance inflation factors.

# Calculate VIF.  
# First, use the corvif function provided by Highland Stats (aka Alain Zuur).
# Function defined in masterLibrary.R.  See script for details.
# Assume a cutoff of 5
# Iteratively remove variable with highest VIF, 
# then recalculate until all VIF < 5
# All uncommented variables have a VIF < 2.12, but corvif gives
# a warning message, results reliable?
corvif(cbind(meanVariance.c.lake.lu$max.depth.ft,
             meanVariance.c.lake.lu$res.perimeter.m,
             meanVariance.c.lake.lu$res.fetch.m,
            # meanVariance.c.lake.lu$reservoir.area.m2, # remove first
            # meanVariance.c.lake.lu$watershed.area.m2, # remove second
             meanVariance.c.lake.lu$percent.agg.ag,
             meanVariance.c.lake.lu$rda,
             meanVariance.c.lake.lu$si))

# Try using VIF function in fsmb package.
# vif_func (defined in masterLibrary.R), is a convenient
# wrapper for fmsb::VIF
# Gives same results as above, with no warning message
vif_func(in_frame = select(meanVariance.c.lake.lu,
                           max.depth.ft, res.perimeter.m,
                           res.fetch.m, reservoir.area.m2,
                           watershed.area.m2, percent.agg.ag, 
                           rda, si),
         thresh = 5, trace = TRUE)

#################################################################
# Set up TOTAL EMISSION RATE model with non correlated variables
m1.T <- lm(ch4.trate.mg.h_Estimate ~ (max.depth.ft + res.perimeter.m +
           res.fetch.m + percent.agg.ag + rda + si)^2, # include 2-way interactions
         data = meanVariance.c.lake.lu)
summary(m1.T);anova(m1.T)  # a few things going
plot(m1.T) # not too bad!

# assess Multi-collinearity of model
sqrt(car::vif(m1.T)) # car, vif>2 indicates problems? (http://www.statmethods.net/stats/rdiagnostics.html)
fmsb::VIF(m1.T) # fmsb, vif < 10, all good here.

# Why to the fmsb and car variance inflation factors give different
# answers?
# Note that the car function gives vif values for interactions.  Should interactions
# be considered in vif_func above.

# Move forward with stepwise model selection
m.T.null <- lm(ch4.trate.mg.h_Estimate ~ 1, data = meanVariance.c.lake.lu)
m.T.step <- step(m.T.null, # model to start with
                   scope = list(lower = m.T.null, upper = m1.T), # range to fit
                   direction ="both")
anova(m.T.step);summary(m.T.step) # positive rda coefficient, p=0.001, r2=0.31




#################################################################
# Set up VOLUMETRIC RATE model with non correlated variables
m1.V <- lm(ebMlHrM2_Estimate ~ (max.depth.ft + res.perimeter.m +
                                res.fetch.m + percent.agg.ag + rda + si)^2, # include 2-way interactions
         data = meanVariance.c.lake.lu)
summary(m1.V);anova(m1.V)  # Lots going on
plot(m1.V) # not too bad!

# Move forward with stepwise model selection
m.V.null <- lm(ebMlHrM2_Estimate ~ 1, data = meanVariance.c.lake.lu)
m.V.step <- step(m.V.null, # model to start with
               scope = list(lower = m.V.null, upper = m1.V), # range to fit
               direction ="both")
anova(m.V.step);summary(m.V.step) # ag p=0.003, rda = 0.07, si = 0.08, r2=0.43

# Calculate Relative Importance for Each Predictor
# https://www.r-bloggers.com/interpreting-regression-coefficient-in-r/
# 3 variables explain 43.35 % of variability
# of that 43%, 56% is due to ag, 32% is due to rda, and 11% is si
calc.relimp(m.V.step, type = "lmg",
            rela=TRUE)

# Relative strenght of variables.  Calculation based on standardized slopes.
std_slope <- function(model, variable) {
  return(coef(model)[variable] * (sd(model$model[[variable]])/sd(model$model[[1]])))
}

std_slope(m.V.step, "percent.agg.ag") # 0.5
std_slope(m.V.step, "rda") # 0.28
std_slope(m.V.step, "si") # 0.26






# TOTAL EMISSIONS BY CHEMISTRY AND PRODUCTIVITY MODELS----------
# 
# chla_Estimate, tp_Estimate, tn_Estimate,

# Check for collinearity, select non correlated variables based
# on variance inflation factors.

# Calculate VIF.  
# First, use the corvif function provided by Highland Stats (aka Alain Zuur).
# Function defined in HighstatLibV6.R.
# All variables have a VIF < 2.81, but corvif gives
# a warning message, results reliable?
with(meanVariance.c.lake.lu, 
     corvif(cbind(chla_Estimate, 
                  tp_Estimate,
                  tn_Estimate)))

# Try using VIF function in fsmb package.
# vif_func (defined in masterLibrary.R), is a convenient
# wrapper for fmsb::VIF
# Gives same results as above, with no warning message
vif_func(in_frame = select(meanVariance.c.lake.lu,
                           chla_Estimate, 
                           tp_Estimate,
                           tn_Estimate),
         thresh = 5, trace = TRUE)


# Set up model with non correlated variables
m.chem1 <- lm(ch4.trate.mg.h_Estimate ~ (chla_Estimate + 
                                           tp_Estimate +
                                           tn_Estimate)^2, # include 2-way interactions
              data = meanVariance.c.lake.lu)
summary(m.chem1);anova(m.chem1)  # nothing looks promising
plot(m.chem1) # normality looks bad

# Move forward with stepwise model selection
m.null <- lm(ch4.trate.mg.h_Estimate ~ 1, data = meanVariance.c.lake.lu)
m.step <- step(m.null, # model to start with
               scope = list(lower = m.null, upper = m.chem1), # range to fit
               direction ="both")
anova(m.step);summary(m.step) # no variables retained


 
