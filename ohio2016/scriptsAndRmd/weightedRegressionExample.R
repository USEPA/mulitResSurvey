# Examples of weighted linear regression
#
# January 12, 2017

library(ggplot2)
library(car)
library(plot3D)
library(plot3Drgl)   

# read data for Neptune
# PIs use scripts to produce meanVariance.c.lake.lu
# dat <- read.delim("ohio2016/output/meanVariance.c.lake.lu.txt", sep = " ")

# short names for things we'll use a lot for Neptune
# skip here
meanVariance.c.lake.lu$ch4.trate.mg.h_Estimate <- meanVariance.c.lake.lu$ch4.trate.mg.h_Estimate
meanVariance.c.lake.lu$se <- meanVariance.c.lake.lu$ch4.trate.mg.h_StdError

# Most common way to weight is with reciprocal of variance
# Reciprocal of SE is another choice
meanVariance.c.lake.lu$wt <- 1/meanVariance.c.lake.lu$ch4.trate.mg.h_StdError^2
meanVariance.c.lake.lu$wtalt <- 1/meanVariance.c.lake.lu$ch4.trate.mg.h_StdError

# just look
pairs(meanVariance.c.lake.lu[, c("ch4.trate.mg.h_Estimate", "percent.agg.ag", 
                                 "max.depth.ft")])

# There are three observations with high max.depth.ft values that 
# could be influential in linear models.  Relationships between
# the response and the individual explanatory variables is pretty
# weak.

# plot error bars on response vs each of two explanatory variables
# (just exploring)
qplot(percent.agg.ag, ch4.trate.mg.h_Estimate,
      data = meanVariance.c.lake.lu, color = wt) +
  geom_errorbar(aes(x = percent.agg.ag, 
                    ymin = ch4.trate.mg.h_LCB95Pct, 
                    ymax = ch4.trate.mg.h_UCB95Pct)) 

qplot(max.depth.ft, ch4.trate.mg.h_Estimate, 
      data = meanVariance.c.lake.lu, color = wt) +
  geom_errorbar(aes(x = max.depth.ft, 
                    ymin = ch4.trate.mg.h_LCB95Pct, 
                    ymax = ch4.trate.mg.h_UCB95Pct)) 

# Models without weights. Explanatory vars don't appear significantly
# different from 0, R^2 is small.  The explanatory variables 
# are not doing a good job of explaining the response.
# Not picking up interaction, so work with simpler model.
lm.out <- lm(ch4.trate.mg.h_Estimate ~ percent.agg.ag * max.depth.ft, 
             data = meanVariance.c.lake.lu)
summary(lm.out)

lm.out <- lm(ch4.trate.mg.h_Estimate ~ percent.agg.ag + max.depth.ft, 
             data = meanVariance.c.lake.lu)
summary(lm.out)

# residuals are not normal
hist(residuals(lm.out))
plot(fitted(lm.out), residuals(lm.out), xlab="Fitted", ylab="Residuals")
abline(h=0, col="red") 
shapiro.test(residuals(lm.out))

# With weights... nothing gained (could try variations)
lm.out <- lm(ch4.trate.mg.h_Estimate ~ percent.agg.ag + max.depth.ft, 
             data = meanVariance.c.lake.lu,
             weights = wt) # weights!
summary(lm.out)  #sad face

# with alternate weights... maybe
# Still not a very good model, R^2 is low, 
# but percent.agg.ag coefficient is (marginally)
# different from 0 statistically
lm.out <- lm(ch4.trate.mg.h_Estimate ~ percent.agg.ag + max.depth.ft, 
             data = meanVariance.c.lake.lu,
             weights = wtalt)
summary(lm.out)

# residuals are not skewed, but diagnostics more challenging
# with weighted regression
hist(residuals(lm.out))
plot(fitted(lm.out), residuals(lm.out), xlab="Fitted", ylab="Residuals")
abline(h=0, col="red") 

# Any better if we log the response? 
lm.out <- lm(log(ch4.trate.mg.h_Estimate) ~ percent.agg.ag + max.depth.ft, 
             data = meanVariance.c.lake.lu)
summary(lm.out)

lm.out <- lm(log(ch4.trate.mg.h_Estimate) ~ percent.agg.ag + max.depth.ft, 
             data = meanVariance.c.lake.lu, 
             weights = wt)
summary(lm.out)

lm.out <- lm(log(ch4.trate.mg.h_Estimate) ~ percent.agg.ag + max.depth.ft, 
             data = meanVariance.c.lake.lu, 
             weights = wtalt)
summary(lm.out)

# These don't look promising to me becasue the coefficient on
# depth is estimated to be greater than 0, which is not what's
# expected.
# Odd that with 1/se^2 weights, max.depth.ft shows up as 
# important, whereas unweighted or with 1/se weights, percent.agg.ag
# appears more important.  


### try to visualize a couple of the models with the meanVariance.c.lake.lua

# Unweighted model 
lm.out <- lm(ch4.trate.mg.h_Estimate ~ percent.agg.ag + max.depth.ft, 
             data = meanVariance.c.lake.lu)
summary(lm.out)
ch4fun <- function(a,d){lm.out$coefficients[1] + lm.out$coefficients[2]*a + lm.out$coefficients[3]*d }

# look at the surface, as a contour plot and as a surface
a <- seq(min(meanVariance.c.lake.lu$percent.agg.ag), max(meanVariance.c.lake.lu$percent.agg.ag), length.out = 11)
d <- seq(min(meanVariance.c.lake.lu$max.depth.ft), max(meanVariance.c.lake.lu$max.depth.ft), length.out = 11)
z <- outer(a,d,ch4fun)

# This won't show up in the markdown document, but if it is run separately,
# it produces an interactive graphic that you can spin around to see better. 
persp3Drgl(a,d,z, xlab = "Ag %", ylab = "Max Depth (Ft)" ,main = "Fitted Surface",
           zlab = "Methane Emission Rate (mg/m^2/h)", ticktype = "detailed")
scatter3Drgl(meanVariance.c.lake.lu$percent.agg.ag, 
             meanVariance.c.lake.lu$max.depth.ft, 
             meanVariance.c.lake.lu$ch4.trate.mg.h_Estimate, 
             add = T, col = "black")


# Weighted model (fitted plane is much flatter because large values
# of the response typically have less weight now)
lm.out <- lm(ch4.trate.mg.h_Estimate ~ percent.agg.ag + max.depth.ft, 
             data = meanVariance.c.lake.lu, weights = wtalt)
summary(lm.out)
ch4fun <- function(a,d){lm.out$coefficients[1] + lm.out$coefficients[2]*a + lm.out$coefficients[3]*d }

# look at the surface, as a contour plot and as a surface
a <- seq(min(meanVariance.c.lake.lu$percent.agg.ag), max(meanVariance.c.lake.lu$percent.agg.ag), length.out = 11)
d <- seq(min(meanVariance.c.lake.lu$max.depth.ft), max(meanVariance.c.lake.lu$max.depth.ft), length.out = 11)
z <- outer(a,d,ch4fun)

# This won't show up in the markdown document, but if it is run separately,
# it produces an interactive graphic that you can spin around to see better. 
persp3Drgl(a,d,z, xlab = "Ag %", ylab = "Max Depth (Ft)" ,main = "Fitted Surface",
           zlab = "Methane Emission Rate (mg/m^2/h)", ticktype = "detailed")
scatter3Drgl(meanVariance.c.lake.lu$percent.agg.ag, 
             meanVariance.c.lake.lu$max.depth.ft, 
             meanVariance.c.lake.lu$ch4.trate.mg.h_Estimate, 
             add = T, col = "black")

