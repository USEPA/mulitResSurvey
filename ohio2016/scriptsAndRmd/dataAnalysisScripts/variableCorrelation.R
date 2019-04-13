# UNTRANSFORMED STATISTICAL MODELS
# Three groups of models will be run:
# 1) national observations + national predictors only
# 2) local observations + national predictors only
# 3) local observations + all predictors

# Should evaluate multicollinearity among these three groups of obs and predictors


# National observations + national predictors only-------------
natObsNatPred <-{select(meanVariance.c.lake.lu.agg, 
                        # Response variables
                        # ebMlHrM2_Estimate,
                        # ch4.trate.mg.h_Estimate,
                        # ch4.drate.mg.m2.h_Estimate,
                        # ch4.erate.mg.h_Estimate,
                        # co2.trate.mg.h_Estimate,
                        # co2.drate.mg.m2.h_Estimate,
                        # co2.erate.mg.h_Estimate,
                        
                        # Local chemistry
                        # chla_Estimate, 
                        # tp_Estimate, 
                        # tn_Estimate, 
                        # dissolved.ch4_Estimate,
                        # dissolved.co2_Estimate,
                        
                        # Local chemistry + bathymetry
                        # hypoxic.frac, 
                        # hypol.frac,
                        
                        # National morphometry
                        max.depth.m,
                        mean.depth.m,
                        depth.ratio,
                        dynamic.ratio,
                        prop.less.3m, 
                        res.perimeter.m, 
                        res.fetch.m, 
                        reservoir.area.m2, 
                        circ,
                        rda,
                        # si, Not included in meanVariance.c.lake.lu.agg due to correlation w/size
                        
                        # National watershed
                        watershed.area.m2, 
                        percent.agg.ag, 
                        nhdPctAg2006Slp20Ws, 
                        nhdPctAg2006Slp10Ws, # ag on high slopes
                        nhdFertWs, # synthetic N fertilizer (kg N/ha/yr)
                        nhdManureWs, # manure application (kg N/ha/yr)
                        nhdNWs, # sume of Fert and Manure (kg N/ha/yr)
                        
                        # National soils
                        # Kffactor: relative index of susceptibility of bare, cultivated soil to particle detachment and transport by rainfall.
                        nhdAgKffactWs, # Mean Kffactor on agricultural land (NLCD 2006)   
                        nhdKffactWs, # Mean Kffactor within watershed
                        nhdOmWs, # Mean organic matter content (% by weight) of soils
                        
                        # National temp and runoff
                        nhdTmean8110Ws, # 30-year normal mean temperature (C°) 
                        nhdRunoffWs # mean runoff (mm)  
) }  # Pull out variables of interest

natObsNatPredCor <- cor(natObsNatPred, # calculate correlation matrix
                    use = "pairwise.complete.obs", 
                    method = "spearman")  # Zuur pg 473

write.table(round(natObsNatPredCor, 2), # conditional formatting in Excel to highlight >=0.6
            "ohio2016/output/natObsNatPredCor.txt")

# Highly correlated pairs
# max depth ~ mean depth 0.88 # eliminate mean depth
# max depth ~ prop.less.3m -0.78

# mean depth ~ prop.less.3m 0.91 # eliminate mean depth
# mean depth ~ nhdRunoffWs 0.6

# res.perimeter ~ fetch 0.65
# res.perimeter ~ reservoir.area 0.87
# res.perimeter ~ circularity -0.85
# res.perimeter ~ watershed area 0.62

# fetch ~ res.area 0.62

# res.area ~ watershed.area 0.68

# circ ~ Tmean 0.66
# rda ~ watershed.area 0.64

# percent.ag ~ Fert 0.97
# percent.ag ~ N 0.93 where N is Fert + manure
# percent.ag ~ AgKffact 0.99
# percent.ag ~ Runoff 0.61

# pctAgSlp20 ~ pctAgSlp10 0.87

# pctAgSlp10 ~ OmWs -0.7 Soil Organic matter values

# nhdFertWs ~ agKffactWs 0.95 Soil erodability factor for Ag soils in WS

# AgKffact ~ Runoff -0.63













# Local observations + national predictors only-----------------
localObsNatPred <-{filter(meanVariance.c.lake.lu.agg,
                        citation == "EPA") %>%
    select( 
           # Response variables
           # ebMlHrM2_Estimate,
           # ch4.trate.mg.h_Estimate,
           # ch4.drate.mg.m2.h_Estimate,
           # ch4.erate.mg.h_Estimate,
           # co2.trate.mg.h_Estimate,
           # co2.drate.mg.m2.h_Estimate,
           # co2.erate.mg.h_Estimate,
           
           # Local chemistry
           # chla_Estimate, 
           # tp_Estimate, 
           # tn_Estimate, 
           # dissolved.ch4_Estimate,
           # dissolved.co2_Estimate,
           
           # Local chemistry + bathymetry
           # hypoxic.frac, 
           # hypol.frac,
           
           # National morphometry
           max.depth.m,
           mean.depth.m,
           depth.ratio,
           dynamic.ratio,
           prop.less.3m, 
           res.perimeter.m, 
           res.fetch.m, 
           reservoir.area.m2, 
           circ,
           rda,
           # si, Not included in meanVariance.c.lake.lu.agg due to correlation w/size
           
           # National watershed
           watershed.area.m2, 
           percent.agg.ag, 
           nhdPctAg2006Slp20Ws, 
           nhdPctAg2006Slp10Ws, # ag on high slopes
           nhdFertWs, # synthetic N fertilizer (kg N/ha/yr)
           nhdManureWs, # manure application (kg N/ha/yr)
           nhdNWs, # sume of Fert and Manure (kg N/ha/yr)
           
           # National soils
           # Kffactor: relative index of susceptibility of bare, cultivated soil to particle detachment and transport by rainfall.
           nhdAgKffactWs, # Mean Kffactor on agricultural land (NLCD 2006)   
           nhdKffactWs, # Mean Kffactor within watershed
           nhdOmWs, # Mean organic matter content (% by weight) of soils
           
           # National temp and runoff
           nhdTmean8110Ws, # 30-year normal mean temperature (C°) 
           nhdRunoffWs # mean runoff (mm)  
    ) }  # Pull out variables of interest

localObsNatPredCor <- cor(localObsNatPred, # calculate correlation matrix
                        use = "pairwise.complete.obs", 
                        method = "spearman")  # Zuur pg 473

write.table(round(localObsNatPredCor, 2), # conditional formatting in Excel to highlight >=0.6
            "ohio2016/output/localObsNatPredCor.txt")

# Highly correlated pairs
# Mean depth is highly correlated with max depth AND prop.less.3m.

# max depth ~ mean depth 0.85  # eliminate mean depth
# max depth ~ dynamic ratio -0.64
# max depth ~ prop.less.3m -0.83

# mean depth ~ dynamic ratio -0.62
# mean depth ~ prop.less.3m -0.92  # eliminate mean depth


# res.perimeter ~ reservoir.area 0.84
# res.perimeter ~ circularity -0.76
# res.perimeter ~ watershed area 0.64

# fetch ~ res.area 0.76
# fetch ~ watershed.area 0.62

# res.area ~ watershed.area 0.70


# rda ~ watershed.area 0.72

# percent.ag ~ Fert 0.96
# percent.ag ~ N 0.97 where N is Fert + manure
# percent.ag ~ AgKffact 0.99  # eliminate AgKffact

# pctAgSlp20 ~ pctAgSlp10 0.84 # eliminate AgSlp20
# pctAgSlp20 ~ OmWs -0.6 Soil Organic matter values # eliminate AgSlp20

# pctAgSlp10 ~ OmWs -0.7 Soil Organic matter values

# nhdFertWs ~ agKffactWs 0.95 Soil erodability factor for Ag soils in WS. # eliminate AgKffact

# Manure ~ Tmean -0.72












# Local observations + all predictors-------------
localObsAllPred <-{filter(meanVariance.c.lake.lu.agg,
                         citation == "EPA") %>%
    select(
      # Response variables
      # ebMlHrM2_Estimate,
      # ch4.trate.mg.h_Estimate,
      # ch4.drate.mg.m2.h_Estimate,
      # ch4.erate.mg.h_Estimate,
      # co2.trate.mg.h_Estimate,
      # co2.drate.mg.m2.h_Estimate,
      # co2.erate.mg.h_Estimate,
      
      # Local chemistry
      chla_Estimate, 
      tp_Estimate, 
      tn_Estimate,
      dissolved.ch4_Estimate,
      dissolved.co2_Estimate,
      
      # Local chemistry + bathymetry
      hypoxic.frac, 
      hypol.frac,
      
      # National morphometry
      max.depth.m,
      mean.depth.m,
      depth.ratio,
      dynamic.ratio,
      prop.less.3m, 
      res.perimeter.m,
      res.fetch.m,
      reservoir.area.m2, 
      circ,
      rda,
      si, #Not included in meanVariance.c.lake.lu.agg due to correlation w/size
      
      # National watershed
      watershed.area.m2,
      percent.agg.ag, 
      nhdPctAg2006Slp20Ws, 
      nhdPctAg2006Slp10Ws, # ag on high slopes
      nhdFertWs, # synthetic N fertilizer (kg N/ha/yr)
      nhdManureWs, # manure application (kg N/ha/yr)
      nhdNWs, # sum of Fert and Manure (kg N/ha/yr)
      
      # National soils
      # Kffactor: relative index of susceptibility of bare, cultivated soil to particle detachment and transport by rainfall.
      nhdAgKffactWs, # Mean Kffactor on agricultural land (NLCD 2006)   
      nhdKffactWs, # Mean Kffactor within watershed
      nhdOmWs, # Mean organic matter content (% by weight) of soils
      
      # National temp and runoff
      nhdTmean8110Ws, # 30-year normal mean temperature (C°) 
      nhdRunoffWs # mean runoff (mm)  
    ) }  # Pull out variables of interest

localObsAllPredCor <- cor(localObsAllPred, # calculate correlation matrix
                       use = "pairwise.complete.obs", 
                       method = "spearman")  # Zuur pg 473

write.table(round(localObsAllPredCor, 2), # conditional formatting in Excel to highlight >=0.6
            "ohio2016/output/localObsAllPredCor.txt")

# Highly correlated pairs
# chla ~ TP 0.85
# chla ~ TN .67
# chla ~ max.depth -0.6

# TN ~ percent.agg 0.79
# TN ~ Fert 0.69
# TN ~ agKffact 0.78


# hypoxic.frac ~ mean.depth 0.6
# hypoxic.frac ~ dynamic.ratio -0.6


# max depth ~ mean depth 0.85
# max depth ~ dynamic.ratio -0.64
# max depth ~ prop.less.3m -0.83

# mean depth ~ dynamic.ratio -0.62
# mean depth ~ prop.less.3m 0.92


# res.perimeter ~ reservoir.area 0.84
# res.perimeter ~ circularity -0.76
# res.perimeter ~ watershed area 0.64

# fetch ~ res.area 0.76
# fetch ~ watershed.area 0.62

# res.area ~ watershed.area 0.7

# rda ~ watershed.area 0.72

# percent.ag ~ Fert 0.96
# percent.ag ~ N 0.97 where N is Fert + Manure
# percent.ag ~ AgKffact 0.99

# pctAgSlp20 ~ pctAgSlp10 0.84
# pctAgSlp20 ~ OmWs -0.6 Soil Organic matter values

# pctAgSlp10 ~ OmWs -0.7 Soil Organic matter values

# nhdFertWs ~ agKffactWs 0.95 Soil erodability factor for Ag soils in WS

# manure ~ Tmean -0.72
# AgKffact ~ Runoff -0.63
