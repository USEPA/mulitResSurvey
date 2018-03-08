ggplot(eqAreaData, aes(trap_n2, trap_ch4.ppm/10000 )) + geom_point()

ggplot(eqAreaData, aes(ebMlHrM2, trap_ch4.ppm/10000 )) + geom_point()

# Acton CH4 vs N2 through time
ggplot(filter(eqAreaData, grepl(pattern = "Acton", Lake_Name)),
       aes(trap_n2, trap_ch4.ppm/10000)) +
  geom_point(aes(color = RtrvDat))

# Acton bubbling rate through time
# Stabilizes by mid season
ggplot(filter(eqAreaData, grepl(pattern = "Acton", Lake_Name)),
       aes(RtrvDat, ebMlHrM2)) +
  geom_boxplot(aes(color = RtrvDat))

# Harsha through time
load("ohio2016/inputData/literatureData/harsha2015bubbleComp.RData")
ggplot(harsha2015, aes(deployment.rdate, ch4.ppm/10000)) + geom_point()
