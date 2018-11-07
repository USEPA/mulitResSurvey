## PREVIOUS SIMULATIONS WERE USED TO IDENTIFY ACCEPTABLE VALUES FOR BF, SHR,
## CV FOLDS, AND TRAIN PROPORTION.  SIMULATIONS ALSO SHOWED THAT osMSE VALUES
## WERE APPROXIMATELY LOG-NORMALLY DISTRIBUTED ACROSS REPEAT RUNS USING ARGUMENT
## VALUES REFERENCED ABOVE.  OUR PROPOSAL IS TO ONLY ACCEPT GBMs THAT HAVE > 1000
## TREES, < maxTREES, AND AN osMSE LOWER THAN THE 75TH PERCENTILE OF osMSE
## VALUES OBSERVED ACROSS 50 REPLICATE RUNS.  THIS APPROACH COULD BE USED TO SELECT
## A SINGLE gbm FOR EACH RESPONSE ~ COVARIATE MODEL.  THERE IS SOME CONCERN, HOWEVER,
## THAT THIS SINGLE MODEL MAY NOT BE 'REPRESENTATIVE' OF ALL THE POTENTIAL MODELS THAT
## MEET THE CRITERIA DESCRIBED ABOVE.  IN THE FOLLOWING CODE WE WILL GENERATE 50
## 'REPLICATE' MODELS USING THE CRITERIA ABOVE.  WE WILL THEN EXAMING THE VARIABLE
## IMPORTANCE RANKINGS TO SEE IF THEY ARE LARGELY CONSISTENT, OR HUGELY VARIABLE.

# ## EDIT:  DECIDED TO USE THE osMSE distribution from this new set of simulations
# # evalKmeansTrainProp.R was used to generate 50 'replicate' models for tCH4.
# # We will use the 75th percentile of the osMSE distribution as an acceptance
# # criteria here.  
# osMseCriteria <- evalGBMKmeansResults %>% # see evalKmeansTrainProp.R
#   filter(cvFolds == 10, trainProp == 0.9) %>% # bf=0.9, shr=0.0005 for all runs
#   summarize(criteria = quantile(osMSE, 0.75)) %>% #75th percentile
#   pull(criteria) # convert to named vector

# Set up some parameters for simulations
nRun <- 5 # 50; number of simulations for each resp ~ cov combination
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
  slice(rep(1:n(), nRun)) %>% # nRun rows for each resp~cov
  arrange(resp, cov, obs)

# Use for loop for simulations.  ~30 minutes to 10 runs.
cList <- list() # empty list to hold results
for(i in 1:10) { #nrow(respCovList); using two groups of 5 for example code development
  message(i) # print progress indicator
  run.name <- paste(respCovList$resp[i],
                     respCovList$cov[i],
                     respCovList$obs[i])
    tmpGBM <- runGBM.v4(resp = respCovList$resp[i], 
                        covarType = respCovList$cov[i], 
                        obs = respCovList$obs[i],
                        nTrees = 20000,
                        seed = 2222+i, bf = 0.9, # different seed each run
                        shr = 0.0005, trainProp = 0.9)
   tmpGBM[["run.name"]] <- run.name
  cList[[i]] <- tmpGBM # assign object to list
}


# Look at distribution of MSE values across nRun replicate runs.
# First, extract osMSE values
osMSEdist <- lapply(cList, function(x) {
  c(x$run.name, x$mse[2,2]) # run name and osMSE
}) %>%
  do.call(rbind,.) %>% # collapse to matrix
  as.data.frame() %>% # coerce to df
  rename("resp.cov" = V1, "osMSE" = V2) %>% # sensible column names
  mutate(osMSE = as.numeric(osMSE))

# Define osMSE at 75th percentile
osMSEcrit <- osMSEdist %>%
  group_by(resp.cov) %>%
  summarize(osMSEcriteria = quantile(osMSE, 0.75))

# Plot os MSE distribution
ggplot(osMSEdist, aes(osMSE)) +
  stat_ecdf() +
  xlab("osMSE") +
  geom_vline(data = osMSEcrit, aes(xintercept = osMSEcriteria, color = "red")) +
  facet_wrap(~resp.cov, scales = "free_y") +
  theme(axis.title.y = element_blank()) +
  guides(color=FALSE)



# Index for list elements that have osMSE > osMSEcrit
# modify to accept different resp~cov models
osMSEiList <- list()
for (i in 1:(length(cList)/nRun)) { # for each resp~cov group
  osMSEiList[[i]] <- lapply(cList[(1+(nRun*(i-1))):(nRun*i)], function(x) { # subset group of gmb w/same resp ~ cov
    x$mse[2,2] < osMSEcrit$osMSEcriteria[i]
  })
}
osMSEi <- unlist(osMSEiList) # convert to vector of logicals

# Filter cList to exclude models that do not meet criteria
cList.2 <- cList[osMSEi] # subset based on logical
length(cList);length(cList.2) # 50 -> 37, 13 models excluded

resp.cov <- sapply(cList.2, function(x) { # res~cov combo in filtered list
  x$run.name
}) 


# Create table of variable importance (vi) rankings 
# Produces a list, each element contains a DF.  Number of columns = # of variables in model
# only 1 row, corresponding to variables.
viTable <- 
lapply(cList.2, function(x) { # apply to each element of list
  x$gbm %>% # apply to gbm object inlist
    summary(plotit = FALSE) %>% # apply summary function, supress plot
    select(var) %>% # extract list of variable importance
    t() %>% # transpose
    as.data.frame() %>% # convert to df
    mutate_all(as.character) %>% # factor -> df
    setNames(paste0("rank", 1:ncol(.))) %>% # rename columns
    mutate(run.name = x$run.name) %>% # add column to df for run.name
    select(run.name, everything()) # this moves run.name to column position 1, keeping all other variables
    }) %>%
  # collapse to df.  rbind.fill accomodates different number of columns among 
  # individual dfs in list.  This occured because 'all' and 'nat' covariates
  # have different number of predictor variables.
  do.call(rbind.fill, .)

# Now we want to operate on data organized by groups of resp ~ cov
# I was having a tough time doing this w/everything in df, so splitting
# back into list.  One list element for each resp~cov combination.
viList = split(viTable, viTable$run.name) # split by run name


# Apply table function to each column of DF.  Each list element is a DF.
# This creates a fairly complicate nested list.
# One element for each resp ~ cov
# One sublist for each rank
viFreqList <- lapply(viList, function(x) {
  apply(X = x, MARGIN = 2, FUN = table)
})

# Convert the sublists for each ranking into a DF for easier viewing.
viFreqList.df <- list() # empty list for results
for (i in 1:2) { # nrow(respCovList)
  viFreqList.df[[i]] <- lapply(viFreqList[i][[1]], function(x) {
  as.data.frame(x)
})
}

viFreqList.df[1] # view results for list element 1.  Change 1 to .. for others
viFreqList.df[2]


####PICK UP HERE!!!!!

# Format data for a cleaner table + stacked bar chart
viTable.g.2 <- gather(viTable) %>% # convert to tidy (i.e. columns)
  table() %>% # calculate frequencies
  as.data.frame(stringsAsFactors = FALSE) %>% # to df, with characters
  mutate(key = factor(x = key, # ordered factors for plotting
                      levels = paste0("rank",1:14), ordered = TRUE)) %>%
  arrange(key) %>% # arrange for easier table viewing
  filter(!(Freq == 0)) # Remove instances of 0 freq to clean things up

# Stacked bar chart with labels
ggplot(viTable.g.2, aes(key, Freq, fill = value, label = value)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

ggsave("ohio2016/output/figures/viDistLocalTch4AllCovar.tiff",
       units="in",  # specify units for dimensions
       width=11,   
       height=8, 
       dpi=600,   
       compression = "lzw")
