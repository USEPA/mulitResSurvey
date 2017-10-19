# SCRIPT FOR CALCULATING OBSERVED AND SATURATED DISSOLVED GAS CONCENTRATIONS
# USES FUNCTIONS FROM NEON dissGas PACKAGE.  AS OF 10/6/2017, PACKAGE IS
# STILL IN DEVELOPMENT PHASE.  WILL PULL FUNCTIONS FROM MY LOCAL FORK (SEE
# masterLibrary.R).  MAY CHANGE LATER.

# def.calc.sdg.R



eqAreaData <- with(eqAreaData, def.calc.sdg(inputFile = eqAreaData, 
                                            volGas = HeVol, volH2O = H2O_vol, 
                                            baro = BrPrssr, 
                                            waterTemp = Tmp_C_S, # lake temp
                                            headspaceTemp = Tmp_C_S, # use lake temp
                                            eqCO2 = dissolved_co2.ppm, 
                                            sourceCO2 = 0, # Used He
                                            airCO2 = 405, # global mean
                                            eqCH4 = dissolved_ch4.ppm, 
                                            sourceCH4 = 0, # Used He 
                                            airCH4 = 1.85, # global mean
                                            eqN2O = dissolved_n2o.ppm, 
                                            sourceN2O = 0, # Used He 
                                            airN2O = 0.33)) %>% # global mean
  mutate(co2.sat.ratio = dissolvedCO2 / satCO2,
         ch4.sat.ratio = dissolvedCH4 / satCH4,
         n2o.sat.ratio = dissolvedN2O / satN2O) %>%
  rename(dissolved.co2 = dissolvedCO2,
         sat.co2 = satCO2,
         dissolved.ch4 = dissolvedCH4,
         sat.ch4 = satCH4,
         dissolved.n2o = dissolvedN2O,
         sat.n2o = satN2O)
