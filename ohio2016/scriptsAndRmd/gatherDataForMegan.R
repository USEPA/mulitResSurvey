# SCRIPT TO GATHER ANCILLARY DATA FOR MEGAN'S SEDIMENT SITES

# Read excel file specifying sediment sampling locations
sedSite <- read_excel(path = "ohio2016/inputData/sedimentData/multiReservoirSedimentSamplingSites.xlsx")

# Merge with site specific data
sedBySite <- merge(sedSite, eqAreaData, all.x = TRUE) %>% 
  select(Lake_Name, siteID, wtrDpth, chla_S, DO__L_S, DOPrc_S, ORP_S, 
         pH_S, SpCn__S, Tmp_C_S, TrNTU_S, co2.drate.mg.h.best, co2.erate.mg.h, 
         co2.trate.mg.h, ch4.drate.mg.h.best, ch4.erate.mg.h, ch4.trate.mg.h, 
         ebMlHrM2)


# Merge with lake estimates
sedByLake <- merge(select(sedSite, Lake_Name) %>% distinct(Lake_Name), 
                   localDataGbm,
                   all.x = TRUE)

# Write to disk for Megan/Ishi
write.table(sedBySite, file = paste0("ohio2016/output/",
                                     "sedBySite", Sys.Date(), ".txt"), 
            row.names = FALSE, col.names = TRUE)

write.table(sedByLake, file = paste0("ohio2016/output/",
                                     "sedByLake", Sys.Date(), ".txt"), 
            row.names = FALSE, col.names = TRUE)
