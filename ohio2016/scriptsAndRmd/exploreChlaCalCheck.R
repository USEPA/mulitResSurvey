# PLOT SONDE BASED CHL A MEASUREMENTS WITH LAB CHECKS
# FROM MULTI RES SURVEY

ggplot(filter(eqAreaData, !is.na(chla.sample)),
              aes(chla.sample, chla_S)) +
  geom_point(aes(color = Lake_Name)) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Lab based chl a (ug/L") +
  ylab("Sonde based chl a (ug/L")


# PLOT SONDE BASED CHL a MEASURMENTS WITH LAB CHECK
# FROM BURNETT WOODS

ggplot(chlCal, aes(chla.sample, sondeChl)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Lab based chl a (ug/L") +
  ylab("Sonde based chl a (ug/L")