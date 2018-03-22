

# See here for Acton Lake data from biweekly sampling
# L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/survey/actonTrapGasComposition.csv 

actWeekly <- read.csv(file = "L:/Priv/Cin/NRMRL/reservoirEbullitionStudy/actonEddyCovariance/survey/actonTrapGasComposition.csv", 
                      as.is = TRUE) %>%
  mutate(deplyDt = as.Date(Rdate),
         Lake_Name = "Acton Lake",
         siteID = ifelse(site == "u14",
                         "U-14",
                         ifelse(site == "u12",
                                "U-12",
                                NA)),
         caseID = paste(deplyDt, siteID)) %>%
  rename(trap_n2o.ppm = n2o.ppm,
         trap_co2.ppm = co2.ppm,
         trap_ch4.ppm = ch4.ppm,
         trap_n2 = n2.perc) %>%
  select(deplyDt, Lake_Name,
         siteID, caseID, trap_n2o.ppm,
         trap_co2.ppm, trap_ch4.ppm,
         trap_n2)

# Take a look at replicates 

ggplot(actWeekly, aes())

actAll <- rbind(filter(eqAreaData, 
                       !is.na(deplyDt),
                       grepl(pattern = "Acton", Lake_Name)) %>%
                  select(deplyDt, Lake_Name,
                         siteID, trap_n2o.ppm,
                         trap_co2.ppm, trap_ch4.ppm,
                         trap_n2),
                actWeekly) %>%
  mutate(deplyDtNoYear = as.Date(paste("1800", 
                                       format(deplyDt, "%m-%d"), 
                                       sep = "-")))


# grts survey data
ggplot(eqAreaData, aes(trap_n2, trap_ch4.ppm/10000 )) + geom_point()

ggplot(eqAreaData, aes(ebMlHrM2, trap_ch4.ppm/10000 )) + geom_point()

# grts survey data
# Acton CH4 vs N2 through time
ggplot(filter(eqAreaData, grepl(pattern = "Acton", Lake_Name)),
       aes(trap_n2, trap_ch4.ppm/10000)) +
  geom_point(aes(color = RtrvDat))

# Acton bubbling rate through time
# Stabilizes by mid season
ggplot(filter(eqAreaData, grepl(pattern = "Acton", Lake_Name)),
       aes(RtrvDat, ebMlHrM2)) +
  geom_boxplot(aes(color = RtrvDat))

# All Acton Lake data, including biweekly sampling
ggplot(actAll, aes(RtrvDat, trap_ch4.ppm/10000)) +
  geom_point()

# U-12 and U-14 Acton Lake data, including biweekly sampling
ggplot(filter(actAll, siteID %in% c("U-12", "U-14")),
              aes(deplyDtNoYear, trap_ch4.ppm/10000)) +
  geom_point(aes(color = siteID)) +
  ylim(0,90) +
  geom_smooth(aes(color = siteID)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle =45),
        axis.title.x = element_blank())

# U-12 and U-14 Acton Lake data, including biweekly sampling
# Excluding 2016 survey
# Shows one U-14 replicate that is too low (Oct 5)
ggplot(filter(actAll, siteID %in% c("U-12", "U-14"),
              deplyDt > as.Date("2017-01-01")),
       aes(deplyDt, trap_ch4.ppm/10000)) +
  geom_point(aes(color = siteID)) +
  ylim(0,90) +
  geom_smooth(aes(color = siteID)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle =45),
        axis.title.x = element_blank())

ggsave(filename = "ohio2016/output/figures/actBubbTimeSeries.tiff")

# Harsha through time
load("ohio2016/inputData/literatureData/harsha2015bubbleComp.RData")
ggplot(harsha2015, aes(deployment.rdate, ch4.ppm/10000)) + 
  geom_point() +
  geom_smooth(aes(color = site))
