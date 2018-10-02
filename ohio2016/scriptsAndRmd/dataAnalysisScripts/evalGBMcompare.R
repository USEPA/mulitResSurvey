# SCRIPT TO EVALUATE THE EFFECT OF THE nGBM ARGUMENT IN evalGBM()
# ON THE REPRODUCABILITY OF osMSE OF GBM MODELS RUN WITH IDENTICAL
# PARAMETERS, BUT DIFFERENT SET.SEED VALUES.

# WORKFLOW IS TO 1) LOAD OBJECTS CREATED WITH evalGBM(), 2) RENAME,
# 3) EXTRACT ELEMENTS OF INTEREST, 4) MANIPULATE AND PLOT

# THE RESPONSE VARIABLE IN THESE RUNS IS TOTAL CH4 EMISSION RATE
# THE PREDICTOR VARIABLES ARE NATIONAL = LOCAL

# LOAD DATA-----------------------
# CH4 TOT, nGBM = 25, RUN BY WALDO
load("C:/Users/JBEAULIE/GitRepository/mulitResSurvey/ohio2016/output/evalLocalCh4trateFull25.2222.RData")
evalLocalCh4trateFull.25.1 <- evalLocalCh4trateFull[[1]]
rm(evalLocalCh4trateFull)
evalLocalCh4trateFull.25.1$run <- "localCh4trateFull.25.1"

# CH4 TOT, nGBM = 25, RUN BY WALDO
load("C:/Users/JBEAULIE/GitRepository/mulitResSurvey/ohio2016/output/evalLocalCh4trateFull25.2345.RData")
evalLocalCh4trateFull.25.2 <- evalLocalCh4trateFull[[1]]
rm(evalLocalCh4trateFull)
evalLocalCh4trateFull.25.2$run <- "localCh4trateFull.25.2"

# CH4 TOT, nGBM = 50, RUN BY BEAULIEU ON VM (2DAYS + 8 HOURS)
load("C:/Users/JBEAULIE/GitRepository/mulitResSurvey/ohio2016/output/evalLocalCh4trateFull50.2856.RData")
evalLocalCh4trateFull.50.1 <- evalLocalCh4trateFull[[1]]
rm(evalLocalCh4trateFull)
evalLocalCh4trateFull.50.1$run <- "localCh4trateFull.50.1"

# CH4 TOT, nGBM = 50, RUN BY BEAULIEU ON VM (2DAYS + 8 HOURS)
load("C:/Users/JBEAULIE/GitRepository/mulitResSurvey/ohio2016/output/evalLocalCh4trateFull50.5679.RData")
evalLocalCh4trateFull.50.2 <- evalLocalCh4trateFull[[1]]
rm(evalLocalCh4trateFull)
evalLocalCh4trateFull.50.2$run <- "localCh4trateFull.50.2"


# DATA MANIPULATION-----------------------
evalAll <- rbind(evalLocalCh4trateFull.25.1, evalLocalCh4trateFull.25.2,
                 evalLocalCh4trateFull.50.1, evalLocalCh4trateFull.50.2)

evalAll <- evalAll %>% select(-isMSE, optTrees)
evalAll[1:4] <- apply(X = evalAll[1:4], MARGIN = 2, as.numeric)
evalAll$model <- paste0("Tot CH4, Local, nGBM", substr(x = evalAll$run, 19, 20))
evalAll$run <- paste0("seed", substr(evalAll$run, 22, 22))

# melt, set up ggplot
evalAll.m <- melt(evalAll, id.vars = c("shr", "bf", "run", "model"))
evalAll.c <- dcast(data = evalAll.m, shr + bf + model ~ run + variable)
axisMax <- max(c(evalAll.c$seed1_osMSE, evalAll.c$seed2_osMSE))



# PLOT-----------------

p1 <- ggplot(evalAll.c, aes(seed1_osMSE, seed2_osMSE)) +
  geom_point() +
  xlim(0, axisMax) +
  ylim(0, axisMax) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~model) 


p2 <- ggplot(filter(meanVariance.c.lake.lu.agg, citation == "EPA"),
             aes(x="1", y=ch4.trate.mg.h_Estimate)) +
  geom_boxplot() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

# Push 3 panels to one page.
tiff(filename="ohio2016/output/figures/nGbmTotCh4.tiff",
     width=8, height=4.5, units="in",
     res=800,compression="lzw")

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,5)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = vplayout(1, 1:4))
print(p2, vp = vplayout(1, 5))

dev.off()


# BF vs osMSE
ggplot(evalAll.c, aes(bf, seed2_osMSE)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~model) 


