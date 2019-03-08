
# Calculate Derived Quantities
meanVariance.c.lake.lu <- mutate(meanVariance.c.lake.lu, 
                                 rda = watershed.area.m2 / reservoir.area.m2,
                                 si = res.perimeter.m / (2*sqrt(pi*reservoir.area.m2)), # use m and m2
                                 circ = ((4*pi*reservoir.area.m2)/(res.perimeter.m^2)), 
                                 depth.ratio = mean.depth.m / max.depth.m,
                                 dynamic.ratio = sqrt(reservoir.area.m2/1000000)/mean.depth.m, # km2/m
                                 percent.agg.ag = percent.pasture.hay + 
                                   percent.cultivated.crops)

# Could calculate residence time, but don't have flow data for many
# systems
# residence.time.yr = (reservoir.volume.m3 * 35.3147)  # convert to ft3
# / (outflow.cfs*60*60*24*365),