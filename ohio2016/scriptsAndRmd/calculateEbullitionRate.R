# First calculate volumetric ebullion rate.  Straightforward operation
# That can be vectorized across the entire df.
eqAreaData <- mutate(eqAreaData, 
                     ebMlHrM2 = TtTrpVl / 
                       (as.numeric(trapRtrvDtTm - trapDeplyDtTm) * 
                          ((3.14*.28^2)))) # diameter = 22.25in=0.56m, r=.28m))

# Mass flux rate must be calculated by Lake.  Will use group_by in dplyr





#32. Custom function with ddply.
# May need to specify a 'translation' data set in function call.
#http://stackoverflow.com/questions/20845409/how-do-i-pass-variables-to-a-custom-function-in-ddply
# d <- data.frame(
#   experiment = as.factor(c("foo", "foo", "foo", "bar", "bar", "bar")),
#   si = runif(6),
#   ti = runif(6)
# )
# ddply(d, .(experiment), function(d.sub) cor.test(d.sub$si, d.sub$ti)$statistic)
# #   experiment         t
# # 1        bar 0.1517205
# # 2        foo 0.3387682



ddply(eqAreaData, .(Lake_Name), function(x) mass.rate(x, choice1 = "ch4"))


 #Still trying to figure this out.                    
                     
