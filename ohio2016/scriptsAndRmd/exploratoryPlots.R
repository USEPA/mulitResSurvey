# EXPLORATORY PLOTS

# GRTS ESTIMATES---------------
# Initial looks at emission rates

# DOT PLOT LAKE SPECIFIC DATA--------------
# Pull out whole lake data
meanVariance.c.lake <- filter(meanVariance.c, Subpopulation == "lake")

##---------------------------------------------------------------------------##
# CH4 rates first
# Highlight Harsha Lake data with color
plotColor <- ifelse(meanVariance.c.lake$Lake_Name == "William H Harsha Lake", "red", "black")

# Diffusive CH4  flux
# Reset plotting order for CH4 diffusion
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "ch4.d")
ggplot(meanVariance.c.lake,
       aes(ch4.drate.mg.m2.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.drate.mg.m2.h_UCB95Pct, 
                     xmin = ch4.drate.mg.m2.h_LCB95Pct), color = plotColor)

# Ebullition CH4 mass flux
# Reset plotting order for CH4 ebullition
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "ch4.e")
ggplot(meanVariance.c.lake,
       aes(ch4.erate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.erate.mg.h_UCB95Pct, 
                     xmin = ch4.erate.mg.h_LCB95Pct), color = plotColor)

# CH4 total rate
# Reset plotting order for CH4 ebullition
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "ch4.t")
ggplot(meanVariance.c.lake,
       aes(ch4.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.trate.mg.h_UCB95Pct, 
                     xmin = ch4.trate.mg.h_LCB95Pct), 
                 color = plotColor) +
  xlab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  theme(axis.title.y = element_blank())  # Eliminate x-axis title

ggsave('ohio2016/output/figures/ch4TotDotChart.tiff',  # export as .tif
units="in",  # specify units for dimensions
width=6,   # 1 column
height=6, # Whatever works
dpi=600,   # ES&T. 300-600 at PLOS One,
compression = "lzw")


##---------------------------------------------------------------------------##
# CO2 rates

# Diffusive CO2  flux
# Reset plotting order for CO2 diffusion
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "co2.d")
ggplot(meanVariance.c.lake,
       aes(co2.drate.mg.m2.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.drate.mg.m2.h_UCB95Pct, 
                     xmin = co2.drate.mg.m2.h_LCB95Pct), color = plotColor)

# CO2 ebullition
# Reset plotting order for CO2 ebullition
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "co2.e")
ggplot(meanVariance.c.lake,
       aes(co2.erate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.erate.mg.h_UCB95Pct, 
                     xmin = co2.erate.mg.h_LCB95Pct), 
                 color = plotColor)

# CO2 total rate
# Reset plotting order for CO2 ebullition
meanVariance.c.lake$fLake_Name <- orderLake(meanVariance.c.lake, choice1 = "co2.t")
ggplot(meanVariance.c.lake,
       aes(co2.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.trate.mg.h_UCB95Pct, xmin = co2.trate.mg.h_LCB95Pct), color = plotColor)


# CORRELATIONS----------------------------
# Merge with landuse data.
# Need to adopt lake names consistent with meanVariance.c.lake
survRes$Lake_Name <- ifelse(survRes$lake.name == "BVR",
                            "brookville lake",
                            ifelse(survRes$lake.name == "BHR",
                                   "buckhorn lake",
                                   ifelse(survRes$lake.name == "ceaser creek lake",
                                          "caesar creek lake",
                                          ifelse(survRes$lake.name == "CFK",
                                                 "carr fork lake",
                                                 ifelse(survRes$lake.name == "CRR",
                                                        "cave run lake",
                                                        survRes$lake.name)))))

# Lake_Name in meanVariance.c.lake must be lowercase
meanVariance.c.lu <- merge(mutate(meanVariance.c.lake, 
                                      lLake_Name = tolower(Lake_Name)),
                               survRes, 
                           by.x = "lLake_Name",
                           by.y = "Lake_Name") %>%
  mutate(rda = watershed.area.m2 / reservoir.area.m2,
         si = res.perimeter.m / reservoir.area.m2)


# CO2 total vs land use
ggplot(meanVariance.c.lu,
       aes(percent.agg.ag, co2.trate.mg.h_Estimate)) +
  geom_point()

# CH4 total vs land use
ggplot(meanVariance.c.lu,
       aes(percent.agg.ag, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("% agricultural land use in watershed")

ggsave('ohio2016/output/figures/ch4byLU.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=4,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# CH4 total vs depth
ggplot(meanVariance.c.lu,
       aes(max.depth.ft, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("maximum depth (ft)")

ggsave('ohio2016/output/figures/ch4byDepth.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=4,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# CH4 total vs watershed:reservoir
ggplot(meanVariance.c.lu,
       aes((watershed.area.m2 / reservoir.area.m2), ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("maximum depth (ft)")


# UNTRANSFORMED STATISTICAL MODELS
# Basic correlation matrix.
cor(select(meanVariance.c.lu, 
           ch4.trate.mg.h_Estimate,
           chla_Estimate, #0.19
           max.depth.ft,
           res.perimeter.m,
           res.fetch.m, #-0.13
           reservoir.area.m2, #-0.19
           watershed.area.m2,#0.39
           rda, #0.56 
           si), # 0.22
    use = "pairwise.complete.obs")

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

# LOG TRANSFORMED DATA----------


