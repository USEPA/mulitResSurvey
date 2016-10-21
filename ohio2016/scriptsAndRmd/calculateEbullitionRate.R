# First calculate volumetric ebullion rate.  Straightforward operation
# That can be vectorized across the entire df.
eqAreaData <- mutate(eqAreaData, 
                     ebMlHrM2 = TtTrpVl / 
                       (as.numeric(trapRtrvDtTm - trapDeplyDtTm) * 
                          ((3.14*.28^2)))) # diameter = 22.25in=0.56m, r=.28m))


                     ebCh4MgM2h = mass.rate()


                     
                     
