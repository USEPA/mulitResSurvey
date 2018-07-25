# SCRIPT TO RUN OPTIMIZED gbm MODELS

# SCRIPT BELOW WORKED WITH PREVIOUS VERSION OF runGbm,
# BUT THIS SIMPLE STRATEGY BROKE WHEN runGBM WAS UPDATED
# TO ACCOMODATE evalGBM() OBJECTS WITH VARYING nGBM VALUES.
# CAN PROBABLY RESURECT THIS SCRIPT AFTER A FINAL nGBM VALUE
# HAS BEEN DETERMINED.  WILL RUN FUNCTION ONE BY ONE FOR NOW.

# FUNCTION DEFINITION----------------
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-TEMPORARILY SHELVED#-#-#-#-#-#-#-#-#-#-

# Define  all combinations of response and predictors
resp <- c("ch4.trate.mg.h_Estimate", "ch4.drate.mg.m2.h_Estimate", "ch4.erate.mg.h_Estimate",
         "co2.trate.mg.h_Estimate", "co2.drate.mg.m2.h_Estimate", "co2.erate.mg.h_Estimate")
covList = c("Full", "Nat")
respCovList <- expand.grid(resp, covList) %>%
  rename(cov = Var2, resp = Var1) 

# For loop to run optimized gbm for all response and predictor combinations
gbmMod <- list()
Sys.time()
for (i in 1:length(respCovList$resp)) {
 # see runGBM.R for function 
  gbmMod[[i]] <- runGBM(resp = as.character(respCovList[i, "resp"]), 
                      covarType = as.character(respCovList[i, "cov"]),
                      file =,
                      obs = if())
}
Sys.time() # 4-8 minutes

# 5, co2.drate.mg.m2.h_Estimate Full not converging.  optTrees = 8903 in eval run,
# 8903 in optimized run.
# 9, co2.trate.mg.h_Estimate National not converging.  optTrees = 7992.4 in eval run,
# 10000 in optimized run.


# Print some relevant output to pdf
pdf("ohio2016/output/figures/gbm.tCh4.pdf", paper = "a4r") # landscape orientation
for (i in 1:length(gbmMod)) {
grid.arrange(gbmMod[[i]]$RelInfPlot, gbmMod[[i]]$predPlot, ncol = 2, nrow = 2) # use to put two plots per page
print(gbmMod[[i]]$PDPlots) }
dev.off()


# Apply function one by one-----------------
gbmMod <- list()

gbmMod[[1]] <- runGBM(resp = "ch4.trate.mg.h_Estimate",
                  covarType = "Full", nTrees = 10000, 
                  seed = 2222, file = "50.2856", obs = "Local")

# Print some relevant output to pdf
pdf("ohio2016/output/figures/gbm.models.pdf", paper = "a4r") # landscape orientation
for (i in 1:length(gbmMod)) {
  grid.arrange(gbmMod[[i]]$RelInfPlot, gbmMod[[i]]$predPlot, ncol = 2, nrow = 2) # use to put two plots per page
  print(gbmMod[[i]]$PDPlots) }
dev.off()

