# UNTRANSFORMED STATISTICAL MODELS
# Basic correlation matrix.
cor(select(meanVariance.c.lake.lu, 
           ebMlHrM2_Estimate,
           ch4.trate.mg.h_Estimate,
           chla_Estimate, #ebMl 0.30. Trate 0.18. TP 0.75
           tp_Estimate, #ebMl 0.33. Trate 0.23.
           tn_Estimate, #ebMl 0.45. Trate 0.16.
           max.depth.ft,
           mean.depth.m.morpho, #ebMl -0.11. Trate -0.15.
           res.perimeter.m, #ebMl -0.03. Trate -0.02.
           res.fetch.m, #ebMl 0.08. Trate -0.14.
           reservoir.area.m2, #ebMl -0.10. Trate -0.18.
           watershed.area.m2, #ebMl 0.29. Trate 0.39.
           percent.agg.ag, #ebMl -0.52. Trate 0.24. Ag and TN are 0.8!
           rda, #ebMl 0.44. Trate 0.57. 
           si), #ebMl 0.07. Trate 0.21.
    use = "pairwise.complete.obs")


# TOTAL AND VOLUMETRIC EMISSIONS BY LANDSCAPE AND MORPHOMETRIC MODELS----------
# First will work with model containing only morphometric and 
# and watershed indices.
# %ag, mean depth, reservoir size, watershed size, perimeter, rda, si, fetch
# Lots of collinearity, select non correlated variables based
# on variance inflation factors.

# Calculate VIF.  
# First, use the corvif function provided by Highland Stats (aka Alain Zuur).
# Function defined in masterLibrary.R.  See script for details.
# Assume a cutoff of 5
# Iteratively remove variable with highest VIF, 
# then recalculate until all VIF < 5
# All uncommented variables have a VIF < 1.56, but corvif gives
# a warning message, results reliable?
corvif(cbind(meanVariance.c.lake.lu$mean.depth.m.morpho,
             meanVariance.c.lake.lu$reservoir.area.m2,  
           # meanVariance.c.lake.lu$watershed.area.m2, # remove first
             meanVariance.c.lake.lu$percent.agg.ag,
             meanVariance.c.lake.lu$rda,
             meanVariance.c.lake.lu$si))

# Try using VIF function in fsmb package.
# vif_func (defined in masterLibrary.R), is a convenient
# wrapper for fmsb::VIF
# Gives same results as above, with no warning message
vif_func(in_frame = select(meanVariance.c.lake.lu,
                           mean.depth.m.morpho, 
                           reservoir.area.m2,
                           watershed.area.m2,
                           percent.agg.ag, 
                           rda, 
                           si),
         thresh = 5, trace = TRUE)

# This is a bit tricky because watershed area is removed.  Might be OK
# because rda was retained.

#################################################################
# Set up TOTAL EMISSION RATE model with non correlated variables
m1.T <- lm(ch4.trate.mg.h_Estimate ~ (mean.depth.m.morpho + reservoir.area.m2 +
           percent.agg.ag + rda + si)^2, # include 2-way interactions
           weights = 1/ch4.trate.mg.h_StdError^2, #inverse of variance
         data = meanVariance.c.lake.lu)
summary(m1.T);anova(m1.T)  # a few things going
plot(m1.T) # yikes!

# assess Multi-collinearity of model
sqrt(car::vif(m1.T)) # car, vif>2 indicates problems? (http://www.statmethods.net/stats/rdiagnostics.html)
fmsb::VIF(m1.T) # fmsb, vif < 10, all good here.

# Why to the fmsb and car variance inflation factors give different
# answers?
# Note that the car function gives vif values for interactions.  Should interactions
# be considered in vif_func above.

# Move forward with stepwise model selection
m.T.null <- lm(ch4.trate.mg.h_Estimate ~ 1, 
               weights = 1/ch4.trate.mg.h_StdError^2, #inverse of variance
               data = meanVariance.c.lake.lu)
m.T.step <- step(m.T.null, # model to start with
                   scope = list(lower = m.T.null, upper = m1.T), # range to fit
                   direction ="both")
anova(m.T.step);summary(m.T.step) # positive rda and percent ag coefficient, p=0.006, adj r2=0.24

# Diagnostics
plot(m.T.step) # Positively skewed residuals.
hist(residuals(m.T.step))
plot(fitted(m.T.step), residuals(m.T.step), xlab="Fitted", ylab="Residuals")
abline(h=0, col="red") 

# Observed vs fitted
m.T.stepDf <- data.frame(mPred = fitted(m.T.step),
                         observed =meanVariance.c.lake.lu$ch4.trate.mg.h_Estimate)

# Model consistently underpredicts, hence skewed residuals
ggplot(m.T.stepDf, aes(observed, mPred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ylab("Predicted")

# CONSIDER LOG TRANSFORMING VARIABLE
m2.T <- lm(log10(ch4.trate.mg.h_Estimate) ~ (mean.depth.m.morpho + reservoir.area.m2 +
                                        percent.agg.ag + rda + si)^2, # include 2-way interactions
           weights = 1/ch4.trate.mg.h_StdError^2, inverse of variance
           data = meanVariance.c.lake.lu)
summary(m2.T);anova(m2.T)  # a few things going
plot(m2.T) # two outliers, but better than before

# Move forward with stepwise model selection
m2.T.null <- lm(log10(ch4.trate.mg.h_Estimate) ~ 1, 
               weights = 1/ch4.trate.mg.h_StdError^2, inverse of variance
               data = meanVariance.c.lake.lu)
m2.T.step <- step(m2.T.null, # model to start with
                 scope = list(lower = m2.T.null, upper = m2.T), # range to fit
                 direction ="both")
anova(m2.T.step);summary(m2.T.step) # positive rda and percent ag coefficient, p=0.01, adj r2=0.22

# Diagnostics
hist(residuals(m2.T.step)) # looks ok
plot(fitted(m2.T.step), residuals(m2.T.step), # a little better, not much 
     xlab="Fitted", ylab="Residuals")
abline(h=0, col="red") 

# Observed vs fitted
m2.T.stepDf <- data.frame(mPred = fitted(m2.T.step),
                         observed = log10(meanVariance.c.lake.lu$ch4.trate.mg.h_Estimate))

# Really not much better
ggplot(m.T.stepDf, aes(observed, mPred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ylab("Predicted")

# MOVE TO gls MODEL
# Define weights as inverse of variance
# Trick for weighting (https://www.r-bloggers.com/a-quick-note-in-weighting-with-nlme/)
wts <- 1/meanVariance.c.lake.lu$ch4.trate.mg.h_StdError^2
m3.T <- gls(ch4.trate.mg.h_Estimate ~ (mean.depth.m.morpho + reservoir.area.m2 +
                                         percent.agg.ag + rda + si)^2, # include 2-way interactions
            weights = ~wts,
            data = meanVariance.c.lake.lu)

# Diagnostics
E.m3.T <- resid(m3.T, type = "normalized")
E.m3.fitted <- fitted(m3.T)
plot(E.m3.T ~ E.m3.fitted) # hmm, a bit better than lm
plot(E.m3.T ~ meanVariance.c.lake.lu$mean.depth.m.morpho) # Only positive residuals in deep lakes
plot(E.m3.T ~ meanVariance.c.lake.lu$reservoir.area.m2) # only 3 large lakes
plot(E.m3.T ~ meanVariance.c.lake.lu$percent.agg.ag) # no pattern
plot(E.m3.T ~ meanVariance.c.lake.lu$rda) # less variance at higher rda
plot(E.m3.T ~ meanVariance.c.lake.lu$si) # only 3 large lakes w/large si

# Add mean.depth.m.morpho as variance covariate












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


# TOTAL VOLUMETRIC RATE FIRST
# Set up model with non correlated variables
m.chem1 <- lm(ebMlHrM2_Estimate ~ (chla_Estimate + 
                                           tp_Estimate +
                                           tn_Estimate)^2, # include 2-way interactions
              data = meanVariance.c.lake.lu)
summary(m.chem1);anova(m.chem1)  # nothing looks promising
plot(m.chem1) # normality looks bad

# Move forward with stepwise model selection
m.step <- step(m.chem1,  direction ="both")
anova(m.step);summary(m.step) # TN retained, r2 = 0.21, p = 0.01


# TOTAL EMISSION RATE
# Set up model with non correlated variables
# Missing TN values is screwing things up, remove NAs before analysis
data.trate.chem <- select(meanVariance.c.lake.lu, ch4.trate.mg.h_Estimate,
                          chla_Estimate, tp_Estimate, tn_Estimate, Lake_Name) %>%
  na.omit()
m.chem1 <- lm(ch4.trate.mg.h_Estimate ~ (chla_Estimate + 
                                           tp_Estimate +
                                           tn_Estimate)^2, # include 2-way interactions
              data = data.trate.chem)
summary(m.chem1);anova(m.chem1)  # nothing looks promising
plot(m.chem1) # normality looks bad

# Move forward with stepwise model selection
m.step <- step(m.chem1, # model to start with
               direction ="both",  na.action = na.omit)
anova(m.step);summary(m.step) # no variables retained


 
