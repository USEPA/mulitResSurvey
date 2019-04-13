## PREVIOUS SIMULATIONS WERE USED TO IDENTIFY ACCEPTABLE VALUES FOR BF, SHR,
## CV FOLDS, AND TRAIN PROPORTION.  SIMULATIONS ALSO SHOWED THAT osMSE VALUES
## WERE APPROXIMATELY LOG-NORMALLY DISTRIBUTED ACROSS REPLICATE RUNS HERE WE ONLY 
## ACCEPT GBMs THAT HAVE OPTIMAL TREES > 1000 TREES, BUT < maxTREES, AND AN osMSE
## LOWER THAN THE 75TH PERCENTILE OF osMSE VALUES OBSERVED ACROSS 50 REPLICATE RUNS.

## THIS APPROACH WILL BE USED TO SELECT A SINGLE gbm FOR EACH RESPONSE ~ COVARIATE MODEL.  
## THERE IS SOME CONCERN, HOWEVER, THAT THIS SINGLE MODEL MAY NOT BE 'REPRESENTATIVE' 
## OF ALL THE POTENTIAL MODELS THAT MEET THE CRITERIA DESCRIBED ABOVE.  IN THE FOLLOWING 
## CODE WE WILL GENERATE 50 'REPLICATE' MODELS USING THE CRITERIA ABOVE.  WE WILL THEN
## EXAMING THE VARIABLE IMPORTANCE RANKINGS TO SEE IF THEY ARE LARGELY CONSISTENT, OR HUGELY
## VARIABLE.  FINALLY WE WILL SELECT THE FINAL MODEL FOR EACH RESP~COV.

## THIS SCRIPT EXECUTED ON VM, TOO SLOW ON EZTECH SYSTEM.  LIST OBJECT CONTAINING ALL MODELS
## (cList) AND 629 'GOOD' MODELS (cList.2) IS TOO LARGE TO LOAD INTO EZTECH SYSTEM.  cList.3
## ONLY CONTAINS 17 'FINAL' MODELS AND CAN BE LOADED INTO EZTECH SYSTEM.

# RUN MODELS------------------------
# Set up some parameters for simulations
nRun <- 50 # number of simulations for each resp ~ cov combination
# Define  all combinations of response and predictors
resp <- c("ch4.trate.mg.h_Estimate", "ch4.drate.mg.m2.h_Estimate", "ch4.erate.mg.h_Estimate",
          "co2.trate.mg.h_Estimate", "co2.drate.mg.m2.h_Estimate", "co2.erate.mg.h_Estimate",
          "ebMlHrM2_Estimate")

covList <- c("All", "Nat") # All is national + local

obs <- c("Local", "Nat") # local in EPA 32, Nat is EPA + lit

# Matrix of above, filter out unused scenarios
respCovList <- expand.grid(resp, covList, obs, stringsAsFactors = FALSE) %>%
  rename(cov = Var2, resp = Var1, obs = Var3) %>%
  filter(obs != "Nat" | cov != "All") %>% # Remove Nat obs w/All cov set
  filter(!grepl(pattern = "co2", x = resp) | # Remove Nat CO2 resp, no data
           obs != "Nat") %>%
  filter(obs != "Nat" | # volumetric ebullition not available at national scale
           !grepl(pattern = "eb", x = resp)) %>%
  slice(rep(1:n(), nRun)) %>%
  arrange(resp, cov)

# Use for loop for simulations
cList <- list() # empty list to hold results
for(i in 1:nrow(respCovList)) { # 
#for (i in 31:35){
  message(i) # print progress indicator
  run.name <- paste(respCovList$resp[i],
                    respCovList$cov[i],
                    respCovList$obs[i])
  tmpGBM <- runGBM.v4(resp = respCovList$resp[i], 
                      covarType = respCovList$cov[i], 
                      obs = respCovList$obs[i],
                      nTrees = 20000,
                      seed = 2222+i, bf = 0.9, # different seed each run
                      shr = 0.0005, trainProp = 0.8)
  tmpGBM[["run.name"]] <- run.name
  cList[[i]] <- tmpGBM # assign object to list
}

# INDENTIFY MODELS THAT MEET ACCEPTANCE CRITERIA-----------------------
# DEPRECATED 2/15/2019
# Look at distribution of MSE values across 50 replicate runs.
# First, extract osMSE values
osMSEdist <- lapply(cList, function(x) {
  x$mse[2,2] # extract osMSE
}) %>%
  do.call(rbind,.) %>% # to vector
  as.data.frame() %>% # to df
  mutate(run.name = with(respCovList, paste(resp, cov, obs, sep = " ")))

# Plot
ggplot(osMSEdist, aes(V1)) +
  stat_ecdf(geom = "step") +
  xlab("osMSE") +
  ggtitle("New simulation") +
  #geom_vline(xintercept = 10.04, color = "red") +
  theme(axis.title.y = element_blank()) +
  facet_wrap(~run.name, scales = "free_x")

# Define osMSE at 75th percentile for each model
osMSEcrit <- osMSEdist %>% 
  group_by(run.name) %>% # group by model
  summarize(osMSEcrit = quantile(V1, 0.75)) # calculate 75th percentile, by group

# Index for list elements that have osMSE > osMSEcrit
osMSEi <- lapply(cList, function(x){
  index.i <- grep(pattern = x$run.name, x = osMSEcrit$run.name) # row in osMSEcrit w/data
  x$mse[2,2] < osMSEcrit[index.i, "osMSEcrit"] # logical
}) %>%
  do.call(rbind, .) # df

# Filter cList to exclude models that do not meet criteria
cList.2 <- cList[osMSEi] # subset based on logical
length(cList);length(cList.2) # 850 -> 629, 221 models excluded

# INSPECT VARIABILITY OF VARIABLE IMPORTANCE RANKINGS---------------------------------------------
# Create table of variable importance (vi) rankings 
viTable <- # 20 minutes
  lapply(cList.2, function(x) { # apply to each element of list
    tmp.df <- x$gbm %>% # apply to gbm object inlist
      summary(plotit = FALSE) %>% # apply summary function, supress plot
      select(var) %>% # extract list of variable importance
      t() %>% # transpose
      as.data.frame() %>% # convert to df
      mutate_all(as.character) %>% # factor -> df
      setNames(paste0("rank", 1:ncol(.))) %>% # rename columns
      mutate(run.name = x$run.name) %>% # add run.name
      select(run.name, everything()) # this moves run.name to column position 1, keeping all other variables
  }) %>%
  # collapse to df.  rbind.fill accomodates different number of columns among 
  # individual dfs in list.  This occured because 'all' and 'nat' covariates
  # have different number of predictor variables.
  do.call(rbind.fill, .) # coerce to df

# Quick view of ranking frequencies
tapply(X = viTable$rank1, INDEX = viTable$run.name, FUN = table)
tapply(X = viTable$rank2, INDEX = viTable$run.name, FUN = table)
tapply(X = viTable$rank3, INDEX = viTable$run.name, FUN = table)
tapply(X = viTable$rank4, INDEX = viTable$run.name, FUN = table)
tapply(X = viTable$rank5, INDEX = viTable$run.name, FUN = table)

# Now we want to operate on data organized by groups of resp ~ cov
# I was having a tough time doing this w/everything in df, so splitting
# back into list.  One list element for each resp~cov combination.
viList = split(viTable, viTable$run.name) %>% # split by run name. each list element is df
  # Format data for a cleaner table + stacked bar chart
  lapply(., function(x) {
    select(x, -run.name) %>%
    gather() %>% # convert to tidy (i.e. columns)
      table() %>% # calculate frequencies
      as.data.frame(stringsAsFactors = FALSE) %>% # to df, with characters
      mutate(key = factor(x = key, # ordered factors for plotting
                          levels = paste0("rank",1:length(allCovar)), ordered = TRUE)) %>%
      arrange(key) %>% # arrange for easier table viewing
      filter(!(Freq == 0)) # Remove instances of 0 freq to clean things up
  })
  

# Stacked bar chart with labels
# Trick below to use list element name in ggtitle.  Typical approach pulls name of
# df (i.e. column names).  This puts each ggplot into a list element.
# stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
viList.plots <- lapply(seq_along(viList), function(i) { # first argument is just list indices
  ggplot(viList[[i]], aes(key, Freq, fill = value, label = value)) + #call df by list element
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    ggtitle(names(viList)[[i]]) # grabs name of list element, rather than names in df!
})

# This code pushes each plot onto a single pdf.
# UNTESTED
pdf("C:/Users/JBEAULIE/gitRepo/mulitResSurvey/ohio2016/output/simulatedVariableImportanceNHD.pdf",
    paper = "a4r", width = 11, height = 8)
viList.plots
dev.off()


# SELECT 'FINAL' MODEL FOR PROJECT BY RANDOMLY GRABBING ONE OF THE TOP PERFORMING MODLES BASED ON osMSE-----------------------
### This approach can result in models with good osMSE, but very biased in-sample predictions.  
### See below for alternative approach.
# cList.2 contains all accepted models (629, 37 models for each resp ~ cov).  Need to find index 
# to randomly grab 1 model from the 37 'good' models for each resp~cov.
# Vector of resp~cov names from each list element
run.name <- lapply(cList.2, function(x) x$run.name) %>% # extract run.names
  do.call(rbind, .) %>% # coerce to matrix
  as.vector() # clean up to vector
run.name.df <- data.frame(run.name = run.name, # create df including column for list element position
                          index = 1:length(run.name))

# For each resp~cov, randomly grab one index (from the 37 possible)
set.seed = 4568 # for reproducibility
model.index <- tapply(X = run.name.df$index, # column of list element positions
                      INDEX = run.name.df$run.name, # group by resp~cov
                      function(x) sample(x,size = 1))  # randomly sample 1 index for each resp~cov
cList.3.NHD <- cList.2[model.index]  # subset chosen 
length(cList.2);length(cList.3.NHD) # went from 629 models to 17!
save(cList.3.NHD, file = "C:/Users/JBEAULIE/gitRepo/mulitResSurvey/ohio2016/output/cList.3.NHD.RData") # write to disk for use on EZTech laptop

# lapply(cList.3.NHD, function(x) mgcv::ls.size(x)) 
# the gbm in each list element is huge, but may be needed later on.

# SELECT BEST osMSE MODEL-----------
# THESE MODELS HAVE GOOD osMSE, BUT OFTEN RELATIVELY HIGH isMSE, OFTEN UNDERPREDICTING IN SAMPLE OBSERVATIONS.
# Grab the best model, based on osMSE, rather than randomly grabbing a "good" one?
# Define minimum osMSE for each model
osMSEmin <- osMSEdist %>% 
  group_by(run.name) %>% # group by model
  summarize(osMSEmin = min(V1)) # calculate 75th percentile, by group

# Index for list elements that have osMSE > osMSEcrit
osMSEmin.i <- lapply(cList, function(x){
  index.i <- grep(pattern = x$run.name, x = osMSEmin$run.name) # row in osMSEcrit w/data
  x$mse[2,2] == osMSEmin[index.i, "osMSEmin"] # logical
}) %>%
  do.call(rbind, .) # df

# Filter cList to select best model based on osMSE
cListNhd.min <- cList[osMSEmin.i] # subset based on logical
length(cListNhd.min);length(cList) # 850 -> 17, 17 best models chosen
save(cListNhd.min, file = "C:/Users/JBEAULIE/gitRepo/mulitResSurvey/ohio2016/output/cListNhd.min.RData") # write to disk for use on EZTech laptop

# SELECT MODEL WITH BEST COMBINED osMSE AND isMSE.----------
# Select models that have both low is and os MSE.
# Define minimum osMSE for each model
mseDist <- lapply(cList, function(x) {
  x$mse[1:2, 2] # extract MSE, fine for choosing best model, but will use standardized RMSE for reporting, comparing across models.
}) %>%
  do.call(rbind,.) %>% # to vector
  as.data.frame() %>% # to df
  rename(isMSE = V1, osMSE = V2) %>%
  mutate(run.name = with(respCovList, paste(resp, cov, obs, sep = " "))) %>%
  group_by(run.name) %>%
  mutate(isRank = rank(isMSE),
         osRank = rank(osMSE),
         rank = isRank + osRank,
         best.i = ifelse(rank == min(rank),
                         TRUE,
                         FALSE)) %>%
  group_by(run.name, rank) %>%
  mutate(tieBreak = abs(rank/2 - osRank) + abs(rank/2 - isRank),
         bestModel.i = ifelse(best.i == TRUE & tieBreak == min(tieBreak),
                              TRUE,
                              FALSE)) 
  # arrange(run.name, rank)  # DO NOT ARRANGE!!! MESSES UP INDEXING BELOW

# Filter cList to select best model based on best combined osMSE and isMSE performance
# Confirm mseDist and cList are in same order
cListRunName <- lapply(cList, function(x) x$run.name) %>% # extract run.names
  do.call(rbind, .) %>% # coerce to matrix
  as.vector()
sum(cListRunName == mseDist$run.name) # should be 850

# cListNhdBestMse <- cList[mseDist$bestModel.i] # subset based on logical.  Model w/all predictors, except si.
cListNhdBestMseRed <- cList[mseDist$bestModel.i] # subset based on logical.  Model w/out fetch, watershed area, perimeter
length(cListNhdBestMseRed);length(cList) # 850 -> 17, 17 best models chosen

# Write to disk for use on EZTech machine
# I have run the model with different predictor variables.  Need to adopt unique file name to segregate these modeling results.
# cListNhdBestMse.RData.  Models including NHD data and all other descriptors, selection based on best MSE (i.e. isMSE + osMSE)
# save(cListNhdBestMse, file = "C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/multiResSurvey/cListNhdBestMse.RData") # write to disk for use on EZTech laptop

# cListNhdBestMseRed.RData.  As above, but with fetch, watershed area, and perimeter excluded. 'Red' for reduced.
# save(cListNhdBestMse, file = "C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/multiResSurvey/cListNhdBestMseRed.RData") # write to disk for use on EZTech laptop

