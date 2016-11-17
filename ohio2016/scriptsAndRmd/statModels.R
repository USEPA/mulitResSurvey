# UNTRANSFORMED STATISTICAL MODELS
# Basic correlation matrix.
cor(select(meanVariance.c.lu, 
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



# ZUUR'S PROTOCOL----------
# First will work with model containing only morphometric and 
# and watershed indices.
# %ag, depth, reservoir size, watershed size, perimeter, rda, si, fetch
# Lots of correlation.  Choose set of non correlated variables based
# on variance inflation factors (VIF; Zuur pg. 386).

# Calculate VIF.  Use a cutoff of 5
# Iteratively remove variable with highest VIF, 
# then recalculate until all VIF < 5
corvif(cbind(meanVariance.c.lu$max.depth.ft,
             meanVariance.c.lu$res.perimeter.m,
             meanVariance.c.lu$res.fetch.m,
            # meanVariance.c.lu$reservoir.area.m2, # remove first
            # meanVariance.c.lu$watershed.area.m2, # remove second
             meanVariance.c.lu$percent.agg.ag,
             meanVariance.c.lu$rda,
             meanVariance.c.lu$si))

# All uncommented variables have a VIF < 2.12
m1 <- lm(ch4.trate.mg.h_Estimate ~ (max.depth.ft + res.perimeter.m +
           res.fetch.m + percent.agg.ag + rda + si)^2, # include 2-way interactions
         data = meanVariance.c.lu)
summary(m1);anova(m1)  # a few things going
plot(m1) # not too bad!






# Independent variables to consider
# %ag, depth, reservoir size, watershed size, perimeter, rda, si, fetch
# TP, Chl a
# TP and chla are correlated: 0.7
# %ag and chla are correlated: 0.5
# %ag and TP are correlated: 0.31

# reservoir area and watershed area are correlated: 0.58
# rda and watershed area are correlated: 0.73
# si is correlated with reservoir area and res.fetch: -0.3, -0.49, respectively
# res area and depth are correlated: 0.48

# First model is based on watershed and morphometric characterisitic,
# no correlated variables included.  Next model will use chem data.
# Can't use res area and depth (they are correlated).
# using depth in first model.
m1 <- lm(ch4.trate.mg.h_Estimate ~ rda * max.depth.ft * percent.agg.ag, 
         data = meanVariance.c.lu.lake)
summary(m1);anova(m1)  # a few things going
plot(m1) # normality not so hot.