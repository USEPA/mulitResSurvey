# MAX DEPTH WAS PROVIDED IN UNITS OF FT, WHEREAS ALL OTHER MORPHOMETRIC
# VARIABLES ARE REPORTED IN METRIC.  THIS IS A ONE-LINE SCRIPT TO EXECUTE
# THE UNIT CONVERSION.  THIS COULD BE DONE IN A LOT OF DIFFERENT PLACES,
# BUT THIS PROVIDES AN EASY TO LOCATE MECHANISM, ALBEIT A BIT OVER THE 
# TO DEDICATE AN ENTIRE .R FILE FOR SUCH A SIMPLE MANIPULATION.

# Take meanVariance.c.lake.lu.agg produced from aggregateActon.R.
meanVariance.c.lake.lu.agg <- mutate(meanVariance.c.lake.lu.agg,
                                     max.depth.m = max.depth.ft / 3.28) %>%
  select(-max.depth.ft)
