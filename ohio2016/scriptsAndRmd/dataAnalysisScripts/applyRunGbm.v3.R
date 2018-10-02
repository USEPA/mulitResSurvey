# SCRIPT TO RUN GBM MODELS

# Script runs runGBM.v3 function which assigns reasonable values for gbm 
# arguments.


# Define  all combinations of response and predictors
resp <- c("ch4.trate.mg.h_Estimate", "ch4.drate.mg.m2.h_Estimate", "ch4.erate.mg.h_Estimate",
         "co2.trate.mg.h_Estimate", "co2.drate.mg.m2.h_Estimate", "co2.erate.mg.h_Estimate",
         "ebMlHrM2_Estimate")

covList <- c("Full", "Nat") # full is national + local

obs <- c("Local", "Nat") # local in EPA 32, Nat is EPA + lit

# Matrix of above, filter out unused scenarios
respCovList <- expand.grid(resp, covList, obs, stringsAsFactors = FALSE) %>%
  rename(cov = Var2, resp = Var1, obs = Var3) %>%
  filter(obs != "Nat" | cov != "Full") %>% # Remove Nat obs w/full cov set
  filter(!grepl(pattern = "co2", x = resp) | # Remove Nat CO2 resp, no data
           obs != "Nat") %>%
  filter(obs != "Nat" | # volumetric ebullition not available at national scale
           !grepl(pattern = "eb", x = resp))


# For loop to run gbm for response and predictor combinations
gbmMod <- list()
Sys.time()
for (i in 1:length(respCovList$resp)) {
 # see runGBM.v1.R for function 
  gbmMod[[i]] <- runGBM.v3(resp = respCovList[i, "resp"], 
                      covarType = respCovList[i, "cov"],
                      obs = respCovList[i, "obs"], # Local or Nat
                      seed = 2385,
                      bf = 0.9,
                      shr = 0.0005) # used is nGBM > 10 in evalGBM run
}
Sys.time() # 22 minutes

# Print some relevant output to pdf
pdf("ohio2016/output/figures/gbm.all.pdf", paper = "a4r") # landscape orientation
for (i in 1:length(gbmMod)) {
grid.arrange(gbmMod[[i]]$RelInfPlot, gbmMod[[i]]$predPlot, ncol = 2, nrow = 2) # use to put two plots per page
print(gbmMod[[i]]$PDPlots) }
dev.off()

# Create table of results
parms1 <- ldply(lapply(X = gbmMod, "[", c("optTrees", "rsq_is")), data.frame) %>%
  mutate(response = respCovList$resp,
         obs = respCovList$obs,
         covariate = respCovList$cov)

parms2 <- ldply(lapply(X = gbmMod, "[", "mse"), data.frame) %>% # mse into df
  mutate(response = rep(respCovList$resp, each = 2), # add unique id
         obs = rep(respCovList$obs, each = 2),
         covariate = rep(respCovList$cov, each =2)) %>%
  melt(.) %>%
  dcast(response + obs + covariate ~ mse.msetype) %>%
  mutate(obs.cov = paste(obs, covariate, sep =""))

parmsTable <- merge(parms1, parms2)

write.table(x = parmsTable, file = "ohio2016/output/allGbmTable.txt", row.names = FALSE)

# Plot of osMSE values
ggplot(parmsTable, aes(obs.cov, out_sample)) +
  geom_bar(stat = "identity") +
  facet_wrap(~response, scale = "free_y")

source("ohio2016/scriptsAndRmd/dataAnalysisScripts/brt.functions.R")
gbm.simplify(gbmMod[[1]]$gdm)
