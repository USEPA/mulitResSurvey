# SCRIPT TO RUN OPTIMIZED gbm MODELS

# Script runs runGBM.v2 function which extracts best parameters from
# objects created with evalGBM function.  The loop works up models
# where evalGBM was run with nGBM = 10.  For instances where nGBM >10,
# this code at the bottom can run the model on a 1x1 basis.


# Define  all combinations of response and predictors
resp <- c("ch4.trate.mg.h_Estimate", "ch4.drate.mg.m2.h_Estimate", "ch4.erate.mg.h_Estimate",
         "co2.trate.mg.h_Estimate", "co2.drate.mg.m2.h_Estimate", "co2.erate.mg.h_Estimate",
         "ebMlHrM2_Estimate")

covList <- c("Full", "Nat") # full is national + local

obs <- c("Local", "Nat") # local in EPA 32, Nat is EPA + lit

# Matrix of above, filter out unused scenarios
respCovList <- expand.grid(resp, covList, obs) %>%
  rename(cov = Var2, resp = Var1, obs = Var3) %>%
  filter(obs != "Nat" | cov != "Full") %>% # Remove Nat obs w/full cov set
  filter(!grepl(pattern = "co2", x = resp) | # Remove Nat CO2 resp, no data
           obs != "Nat") %>%
  filter(obs != "Nat" | # volumetric ebullition not available at national scale
           !grepl(pattern = "eb", x = resp))


# For loop to run optimized gbm for response and predictor combinations
gbmMod <- list()
Sys.time()
for (i in 1:length(respCovList$resp)) {
 # see runGBM.v1.R for function 
  gbmMod[[i]] <- runGBM.v2(resp = as.character(respCovList[i, "resp"]), 
                      covarType = as.character(respCovList[i, "cov"]),
                      obs = as.character(respCovList[i, "obs"]), # Local or Nat
                      file ="") # used is nGBM > 10 in evalGBM run
}
Sys.time() # 4-8 minutes

# Print some relevant output to pdf
pdf("ohio2016/output/figures/gbm.all.pdf", paper = "a4r") # landscape orientation
for (i in 1:length(gbmMod)) {
grid.arrange(gbmMod[[i]]$RelInfPlot, gbmMod[[i]]$predPlot, ncol = 2, nrow = 2) # use to put two plots per page
print(gbmMod[[i]]$PDPlots) }
dev.off()


# Apply function one by one for evalGbm nGbm >10-----------------
gbmMod <- list()

gbmMod[[1]] <- runGBM.v2(resp = "ch4.trate.mg.h_Estimate",
                  covarType = "Full", nTrees = 10000, 
                  seed = 2222, file = "50.2856", obs = "Local")

# Print some relevant output to pdf
pdf("ohio2016/output/figures/gbm.tCh4.pdf", paper = "a4r") # landscape orientation
for (i in 1:length(gbmMod)) {
  grid.arrange(gbmMod[[i]]$RelInfPlot, gbmMod[[i]]$predPlot, ncol = 2, nrow = 2) # use to put two plots per page
  print(gbmMod[[i]]$PDPlots) }
dev.off()





