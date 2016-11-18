# UNTRANSFORMED STATISTICAL MODELS
# Basic correlation matrix.
cor(select(meanVariance.c.lu, 
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

# START WITH SIMPLE MODELS############################################
# Initial hypothesis: land use * depth
m1 <- lm(ch4.trate.mg.h_Estimate ~ percent.agg.ag * max.depth.ft, 
         data = meanVariance.c.lu)
summary(m1) # nothing

# Chl a
m2 <- lm(ch4.trate.mg.h_Estimate ~ chla_Estimate * max.depth.ft, 
         data = meanVariance.c.lu)
summary(m2) # nothing

# Reservoir and watershed size
# Watershed size, p = 0.01
m3 <- lm(ch4.trate.mg.h_Estimate ~ watershed.area.m2, 
         data = meanVariance.c.lu.lake)
summary(m3) 

# Reservoir area, p=0.31
m4 <- lm(ch4.trate.mg.h_Estimate ~ reservoir.area.m2, 
         data = meanVariance.c.lu)
summary(m4) 

# Watershed Area : Reservoir area, p = 0.001, r2=0.31!
m5 <- lm(ch4.trate.mg.h_Estimate ~ I(watershed.area.m2 / reservoir.area.m2), 
         data = meanVariance.c.lu)
summary(m5) 

# Watershed Area : Reservoir area * land use, nothing!
m6 <- lm(ch4.trate.mg.h_Estimate ~ I(watershed.area.m2 / reservoir.area.m2) *
           percent.agg.ag, 
         data = meanVariance.c.lu)
summary(m6) 

# Watershed Area : Reservoir area * depth, depth + interaction!
m7 <- lm(ch4.trate.mg.h_Estimate ~ I(watershed.area.m2 / reservoir.area.m2) *
           max.depth.ft, 
         data = meanVariance.c.lu)
summary(m7)


# Watershed Area : Reservoir area * depth * ag, depth + interaction!
m8 <- lm(ch4.trate.mg.h_Estimate ~ I(watershed.area.m2 / reservoir.area.m2) *
           max.depth.ft * percent.agg.ag, 
         data = meanVariance.c.lu)
summary(m8)
m9 <- update(m8, .~. -I(watershed.area.m2/reservoir.area.m2):max.depth.ft:percent.agg.ag)
anova(m8, m9) # p =0.39, remove 3-way
summary(m9)
m10 <- update(m9, .~. -I(watershed.area.m2/reservoir.area.m2):percent.agg.ag)
anova(m9, m10) # p = 0.99
m11 <- update(m10, .~. -max.depth.ft:percent.agg.ag)
anova(m10, m11) #p = 0.78
m12 <- update(m11, .~. -percent.agg.ag)  # remove LU
anova(m11, m12) #p=0.21
summary(m12) #max depth, watershed/lake * max depth)


# 3D surface plot for depth and watershed:reservoir area
summary(out <- lm(ch4.trate.mg.h_Estimate ~ I(watershed.area.m2 / reservoir.area.m2) *
                    max.depth.ft, 
                  data = meanVariance.c.lu))

# function for methane emission
ch4fun <- function(a,d){out$coefficients[1] + out$coefficients[2]*a + out$coefficients[3]*d + out$coefficients[4]*a*d}

# look at the surface, as a contour plot and as a surface
a <- with(meanVariance.c.lu, 
          seq(min(watershed.area.m2 / reservoir.area.m2), 
              max(watershed.area.m2 / reservoir.area.m2), 
              length.out = 11))
d <- seq(min(meanVariance.c.lu$max.depth.ft), 
         max(meanVariance.c.lu$max.depth.ft), 
         length.out = 11)
z <- outer(a,d,ch4fun)

#3D surface plot
#Can't do in R3.3.0
persp3D(x = a, y = d, z, phi = 15, theta = 30, main = "Modeled Response Surface", 
        xlab = "watershed:reservoir surface area", 
        ylab = "Max Depth",
        zlab = "Methane emission rate",
        ticktype = "detailed")

#Interactive 3D surface plot
persp3d(x = a, y = d, z, phi = 15, theta = 30, 
        xlab = "watershed:reservoir surface area", 
        ylab = "Max Depth",
        zlab = "Methane emission rate",
        ticktype = "detailed")



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
corvif(cbind(meanVariance.c.lu$max.depth.ft,
             meanVariance.c.lu$res.perimeter.m,
             meanVariance.c.lu$res.fetch.m,
            # meanVariance.c.lu$reservoir.area.m2, # remove first
            # meanVariance.c.lu$watershed.area.m2, # remove second
             meanVariance.c.lu$percent.agg.ag,
             meanVariance.c.lu$rda,
             meanVariance.c.lu$si))

# Try using VIF function in fsmb package.
# vif_func (defined in masterLibrary.R), is a convenient
# wrapper for fmsb::VIF
# Gives same results as above, with no warning message
vif_func(in_frame = select(meanVariance.c.lu,
                           max.depth.ft, res.perimeter.m,
                           res.fetch.m, reservoir.area.m2,
                           watershed.area.m2, percent.agg.ag, 
                           rda, si),
         thresh = 5, trace = TRUE)

#################################################################
# Set up TOTAL EMISSION RATE model with non correlated variables
m1.T <- lm(ch4.trate.mg.h_Estimate ~ (max.depth.ft + res.perimeter.m +
           res.fetch.m + percent.agg.ag + rda + si)^2, # include 2-way interactions
         data = meanVariance.c.lu)
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
m.T.null <- lm(ch4.trate.mg.h_Estimate ~ 1, data = meanVariance.c.lu)
m.T.step <- step(m.T.null, # model to start with
                   scope = list(lower = m.T.null, upper = m1.T), # range to fit
                   direction ="both")
anova(m.T.step);summary(m.T.step) # positive rda coefficient, p=0.001, r2=0.31




#################################################################
# Set up VOLUMETRIC RATE model with non correlated variables
m1.V <- lm(ebMlHrM2_Estimate ~ (max.depth.ft + res.perimeter.m +
                                res.fetch.m + percent.agg.ag + rda + si)^2, # include 2-way interactions
         data = meanVariance.c.lu)
summary(m1.V);anova(m1.V)  # Lots going on
plot(m1.V) # not too bad!

# Move forward with stepwise model selection
m.V.null <- lm(ebMlHrM2_Estimate ~ 1, data = meanVariance.c.lu)
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
with(meanVariance.c.lu, 
     corvif(cbind(chla_Estimate, 
                  tp_Estimate,
                  tn_Estimate)))

# Try using VIF function in fsmb package.
# vif_func (defined in masterLibrary.R), is a convenient
# wrapper for fmsb::VIF
# Gives same results as above, with no warning message
vif_func(in_frame = select(meanVariance.c.lu,
                           chla_Estimate, 
                           tp_Estimate,
                           tn_Estimate),
         thresh = 5, trace = TRUE)


# Set up model with non correlated variables
m.chem1 <- lm(ch4.trate.mg.h_Estimate ~ (chla_Estimate + 
                                           tp_Estimate +
                                           tn_Estimate)^2, # include 2-way interactions
              data = meanVariance.c.lu)
summary(m.chem1);anova(m.chem1)  # nothing looks promising
plot(m.chem1) # normality looks bad

# Move forward with stepwise model selection
m.null <- lm(ch4.trate.mg.h_Estimate ~ 1, data = meanVariance.c.lu)
m.step <- step(m.null, # model to start with
               scope = list(lower = m.null, upper = m.chem1), # range to fit
               direction ="both")
anova(m.step);summary(m.step) # no variables retained


 
