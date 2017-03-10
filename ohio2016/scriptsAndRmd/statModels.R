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
# See 'weights' description in gls help page
wts <- 1/meanVariance.c.lake.lu$ch4.trate.mg.h_StdError^2
vf1 <- varFixed(~wts)
m3.T <- gls(ch4.trate.mg.h_Estimate ~ (mean.depth.m.morpho + reservoir.area.m2 +
                                         percent.agg.ag + rda + si)^2, # include 2-way interactions
            weights = vf1, # weighted by inverse of variance
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
vf2 <- varFixed(~mean.depth.m.morpho)
vf3 <- varComb(vf1, vf2)
m3.T.a <- gls(ch4.trate.mg.h_Estimate ~ (mean.depth.m.morpho + reservoir.area.m2 +
                                           percent.agg.ag + rda + si)^2, # include 2-way interactions
              weights = vf3, 
              data = meanVariance.c.lake.lu)

# Diagnostics
E.m3.T.a <- resid(m3.T.a, type = "normalized")
m3.T.a.fitted <- fitted(m3.T.a)
plot(E.m3.T.a ~ E.m3.fitted) # hmm, a bit better than lm
plot(E.m3.T.a ~ meanVariance.c.lake.lu$mean.depth.m.morpho) # Only positive residuals in deep lakes
plot(E.m3.T.a ~ meanVariance.c.lake.lu$reservoir.area.m2) # only 3 large lakes
plot(E.m3.T.a ~ meanVariance.c.lake.lu$percent.agg.ag) # no pattern
plot(E.m3.T.a ~ meanVariance.c.lake.lu$rda) # less variance at higher rda
plot(E.m3.T.a ~ meanVariance.c.lake.lu$si) # only 3 large lakes w/large si


# Add reservoir.area.m2 as variance covariate
vf4 <- varFixed(~reservoir.area.m2)
vf5 <- varComb(vf1, vf4)
m3.T.b <- gls(ch4.trate.mg.h_Estimate ~ (mean.depth.m.morpho + reservoir.area.m2 +
                                           percent.agg.ag + rda + si)^2, # include 2-way interactions
              weights = vf5, 
              data = meanVariance.c.lake.lu)

# Diagnostics
E.m3.T.b <- resid(m3.T.b, type = "normalized")
m3.T.b.fitted <- fitted(m3.T.b)
plot(E.m3.T.b ~ E.m3.fitted) # hmm, a bit better than lm
plot(E.m3.T.b ~ meanVariance.c.lake.lu$mean.depth.m.morpho) # Only positive residuals in deep lakes
plot(E.m3.T.b ~ meanVariance.c.lake.lu$reservoir.area.m2) # only 3 large lakes
plot(E.m3.T.b ~ meanVariance.c.lake.lu$percent.agg.ag) # no pattern
plot(E.m3.T.b ~ meanVariance.c.lake.lu$rda) # less variance at higher rda
plot(E.m3.T.b ~ meanVariance.c.lake.lu$si) # only 3 large lakes w/large si


# Continuous variance covariates not improving model, try different residual
# spread for each lake.
vf6 <- varIdent(~Lake_Name)
vf7 <- varComb(vf1, vf6)
m3.T.c <- gls(ch4.trate.mg.h_Estimate ~ (mean.depth.m.morpho + reservoir.area.m2 +
                                           percent.agg.ag + rda + si)^2, # include 2-way interactions
              weights = vf7, 
              data = meanVariance.c.lake.lu)

# Diagnostics
E.m3.T.c <- resid(m3.T.c, type = "normalized")
m3.T.c.fitted <- fitted(m3.T.c)
plot(E.m3.T.c ~ E.m3.fitted) # hmm, a bit better than lm
plot(E.m3.T.c ~ meanVariance.c.lake.lu$mean.depth.m.morpho) # Only positive residuals in deep lakes
plot(E.m3.T.c ~ meanVariance.c.lake.lu$reservoir.area.m2) # only 3 large lakes
plot(E.m3.T.c ~ meanVariance.c.lake.lu$percent.agg.ag) # no pattern
plot(E.m3.T.c ~ meanVariance.c.lake.lu$rda) # less variance at higher rda
plot(E.m3.T.c ~ meanVariance.c.lake.lu$si) # only 3 large lakes w/large si

# That didn't work.  Try adding Lake_Name as predictor variable.  Not enough
# DoF to support Lake_Name as main effect.  Try random effect.
# Need lme for 'random'
m4.T <- lme(ch4.trate.mg.h_Estimate ~ (mean.depth.m.morpho + reservoir.area.m2 +
                                           percent.agg.ag + rda + si)^2, # include 2-way interactions
              weights = vf1, # using vf7 (includes ~Lake_name made no differenc)
            random = ~1 | Lake_Name,
              data = meanVariance.c.lake.lu)

# Diagnostics
E.m4.T <- resid(m4.T, type = "normalized")
m4.T.fitted <- fitted(m4.T)
plot(E.m4.T ~ m4.T.fitted) # hmm, a bit better than what we saw above
plot(E.m4.T ~ meanVariance.c.lake.lu$mean.depth.m.morpho) # Only positive residuals in deep lakes
plot(E.m4.T ~ meanVariance.c.lake.lu$reservoir.area.m2) # only 3 large lakes
plot(E.m4.T ~ meanVariance.c.lake.lu$percent.agg.ag) # no pattern
plot(E.m4.T ~ meanVariance.c.lake.lu$rda) # less variance at higher rda
plot(E.m4.T ~ meanVariance.c.lake.lu$si) # only 3 large lakes w/large si


# Better.  Try adding form =~ fitted.
vf8 <- varPower(form =~fitted(.))
vf9 <- varComb(vf1, vf8)
m4.T.a <- lme(ch4.trate.mg.h_Estimate ~ (mean.depth.m.morpho + reservoir.area.m2 +
                                         percent.agg.ag + rda + si)^2, # include 2-way interactions
            weights = vf9, 
            random = ~1 | Lake_Name,
            data = meanVariance.c.lake.lu)

# Diagnostics
E.m4.T.a <- resid(m4.T.a, type = "normalized")
m4.T.a.fitted <- fitted(m4.T.a)
plot(E.m4.T.a ~ m4.T.fitted) # values are much smaller, with a few relatively large magnitude negatve close to zero
plot(E.m4.T.a ~ meanVariance.c.lake.lu$mean.depth.m.morpho) # negative values @4
plot(E.m4.T.a ~ meanVariance.c.lake.lu$reservoir.area.m2) # only 3 large lakes
plot(E.m4.T.a ~ meanVariance.c.lake.lu$percent.agg.ag) # no pattern
plot(E.m4.T.a ~ meanVariance.c.lake.lu$rda) # less variance at higher rda
plot(E.m4.T.a ~ meanVariance.c.lake.lu$si) # only 3 large lakes w/large si

anova(m4.T, m4.T.a) # m4.T is the winner, for now.

# MODEL SELECTION PROCEDURE
# No multi level factors as predictor variables; should be able to use
# t-statistic from summary function to select variables (Zuur pg. 91).
# How should the summary tool be used for model selection?  Remove all non-significant
# variables from full model?  Remove highest order interactions first?

summary(m4.T) # none of the two way interactions are significant
m4.T.1 <- lme(ch4.trate.mg.h_Estimate ~ mean.depth.m.morpho + reservoir.area.m2 +
                                           percent.agg.ag + rda + si, # include 2-way interactions
              weights = vf1, 
              random = ~1 | Lake_Name,
              data = meanVariance.c.lake.lu)

summary(m4.T.1) # only rda is significant (p-value = 0.012)
m4.T.2 <- update(m4.T.1, .~. -mean.depth.m.morpho)
summary(m4.T.2) # still only rda significant (p-value = 0.0103)
m4.T.3 <- update(m4.T.2, .~. -percent.agg.ag)
summary(m4.T.3) # still only rda significant (p-value = 0.0022)
m4.T.4 <- update(m4.T.3, .~. -si)
summary(m4.T.4) # still only rda significant (p-value = 0.0009)
m4.T.5 <- update(m4.T.4, .~. -reservoir.area.m2) # p = 0.0007
summary(m4.T.5)
anova(m4.T.5) # p < 0.001?  How to asses overall model p-value
rsquared(m4.T.5) # function from piecewiseSEM for r2 of lme model r2 (Marginal?) = 0.313
?rsquared

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


# TOTAL EMISSIONS BY ALL (MORPHOMETRY, CHEMISTRY AND PRODUCTIVITY) MODELS----------
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
     corvif(cbind(chla_Estimate, #3.9
                  tp_Estimate, #3.1
                  tn_Estimate, #3.29
                  mean.depth.m.morpho,
                  reservoir.area.m2,  
                 # watershed.area.m2, # remove first
                  percent.agg.ag,  #4.24
                  rda,
                  si)))



# Try using VIF function in fsmb package.
# vif_func (defined in masterLibrary.R), is a convenient
# wrapper for fmsb::VIF
# Gives same results as above, with no warning message
vif_func(in_frame = select(meanVariance.c.lake.lu,
                           chla_Estimate, 
                           tp_Estimate,
                           tn_Estimate,
                           mean.depth.m.morpho,
                           reservoir.area.m2,  
                         #  watershed.area.m2, # remove first
                           percent.agg.ag,
                           rda,
                           si),
         thresh = 5, trace = TRUE)

#after dropping watershed area, all VIF <5, but chemistry are >3 (3.9, 3.1, 3.3), and percent ag is 4.24


# TOTAL VOLUMETRIC RATE FIRST
# Set up model with VIF<5 variables
m.tot1 <- lm(ebMlHrM2_Estimate ~ (chla_Estimate + 
                                     tp_Estimate +
                                     tn_Estimate+
                                     mean.depth.m.morpho +
                                     reservoir.area.m2 +
                                     #percent.agg.ag +
                                     rda +
                                     si), # no 2-way interactions
              data = meanVariance.c.lake.lu)
summary(m.tot1) # std error, t-value, Pr values all NA
                # after removing percent ag (VIF was 4.24), values are populated
                # still no p-values close to 0.1
anova(m.tot1)  # tn, rda, mean depth <0.1
plot(m.tot1) # before removing percent ag:Error in qqnorm.default(rs, main = main, ylab = ylab23, ylim = ylim, ...) : 
                     # y is empty or has only NAs

# Move forward with stepwise model selection
m.step.tot <- step(m.tot1,  direction ="both")
anova(m.step.tot)  #don't trust the p-values
summary(m.step.tot) # several variables with low p-values, r2 = 0.86

## remove variable with highest p-val: tp (p = 0.37)
m.tot2 <- lm(ebMlHrM2_Estimate ~ (chla_Estimate + 
                                    #tp_Estimate +
                                    tn_Estimate+
                                    mean.depth.m.morpho +
                                    reservoir.area.m2 +
                                    #percent.agg.ag +
                                    rda +
                                    si)^2, # include 2-way interactions
             data = meanVariance.c.lake.lu)
summary(m.tot2) # tn, chla:mean depth <0.1
anova(m.tot2)  # tn, rda, mean depth <0.1
plot(m.tot2) # before removing percent ag:Error in qqnorm.default(rs, main = main, ylab = ylab23, ylim = ylim, ...) : 
# y is empty or has only NAs

# Move forward with stepwise model selection
m.step.tot2 <- step(m.tot2,  direction ="both")
anova(m.step.tot2)
summary(m.step.tot2) 


# TOTAL EMISSION RATE
# Set up model with non correlated variables
# Missing TN values is screwing things up, remove NAs before analysis
data.trate.chem <- select(meanVariance.c.lake.lu, ch4.trate.mg.h_Estimate, ch4.trate.mg.h_StdError,
                          chla_Estimate, tp_Estimate, tn_Estimate, Lake_Name,
                          mean.depth.m.morpho, reservoir.area.m2, percent.agg.ag, rda, si) %>%
  na.omit()
m.tot3 <- lm(ch4.trate.mg.h_Estimate ~ (chla_Estimate + 
                                           tp_Estimate +
                                           tn_Estimate +
                                          mean.depth.m.morpho +
                                          reservoir.area.m2 +
                                          #percent.agg.ag +
                                          rda +
                                          si)^2, # include 2-way interactions
              data = data.trate.chem)
summary(m.tot3)
anova(m.tot3)  # only rda
plot(m.tot3) # normality looks bad

# Move forward with stepwise model selection
m.step.tot3 <- step(m.tot3, # model to start with
               direction ="both",  na.action = na.omit)
anova(m.step.tot3);summary(m.step.tot3) # no variables retained
##########################################################
##GLS model


wts <- 1/data.trate.chem$ch4.trate.mg.h_StdError^2
vf1 <- varFixed(~wts)
m.T.chem <- gls(ch4.trate.mg.h_Estimate ~ (mean.depth.m.morpho +
                                           chla_Estimate + 
                                           tp_Estimate +
                                           tn_Estimate +
                                           reservoir.area.m2 +
                                          # percent.agg.ag +
                                           rda +
                                           si)^2, # include 2-way interactions
            weights = vf1, # weighted by inverse of variance
            data = data.trate.chem)

# Diagnostics
E.m.T.chem <- resid(m.T.chem, type = "normalized")
E.m3.fitted <- fitted(m.T.chem)
plot(E.m.T.chem ~ E.m3.fitted) # hmm, a bit better than lm
plot(E.m.T.chem ~ data.trate.chem$mean.depth.m.morpho) # Only positive residuals in deep lakes
plot(E.m.T.chem ~ data.trate.chem$reservoir.area.m2) # only 3 large lakes
plot(E.m.T.chem ~ data.trate.chem$percent.agg.ag) # no pattern
plot(E.m.T.chem ~ data.trate.chem$rda) # less variance at higher rda
plot(E.m.T.chem ~ data.trate.chem$si) # only 3 large lakes w/large si



 
