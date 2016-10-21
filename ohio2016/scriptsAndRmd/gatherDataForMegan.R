# SCRIPT TO GATHER ANCILLARY DATA FOR MEGAN'S SEDIMENT SITES

# Read excel file specifying sediment sampling locations
sedSite <- read_excel(path = "ohio2016/inputData/multiReservoirSedimentSamplingSites.xlsx")

# Merge with site specific data
sedBySite <- merge(sedSite, eqAreaData, all.x = TRUE) %>% 
  select(Lake_Name, siteID, Area_km2, chla_S, DO__L_S, DOPrc_S, ORP_S, pH_S, SpCn__S,
         Tmp_C_S, TrNTU_S, co2.drate.mg.h.best, ch4.drate.mg.h.best, ebMlHrM2)


# Merge with lake estimates
sedByLake <- merge(select(sedSite, Lake_Name) %>% distinct(Lake_Name), 
                   filter(meanVariance, Subpopulation == "lake"),
                   all.x = TRUE)

# Write to disk for Megan/Ishi
write.table(sedBySite, file = "ohio2016/output/sedBySite.txt", 
            row.names = FALSE, col.names = TRUE)

write.table(sedByLake, file = "ohio2016/output/sedByLake.txt", 
            row.names = FALSE, col.names = TRUE)
