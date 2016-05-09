# READ IN NID DATA SUPPLIED TO DYANMAC FOR WATERSHED CHARACTERIZATION
# DATA FILE CONTAINS LAT AND LONG OF OHIO NID RESERVOIRS




nid.lat.long <- read.table("L:/Lab/GISData/GIS-User/Damico-Golden-Prues/aprues/Jake_Beaulieu/GHG_Emissions/originals/OhioReservoirsNID.txt",
                           as.is = TRUE)
str(nid.lat.long)
str(survRes3)

# It looks like I can merge the surveydesign file with nid.lat.long using dam and DAM_NAME, respectively
# Select columns of interest from nid.lat.long.  Need to format names identically.
nid.lat.long <- select(nid.lat.long, DAM_NAME, LONGITUDE, LATITUDE) %>%
  rename(dam = DAM_NAME) %>%
  mutate(dam = tolower(dam))

names(nid.lat.long) = tolower(names(nid.lat.long)) # Change all names to all lowercase letters

# Merge
survRes3latLong <- merge(survRes3, nid.lat.long, by = "dam", all.x = TRUE)

# Which observations don't have lat and long?
filter(survRes3latLong, is.na(latitude)) %>% select(dam, lake.name)

# I got the lat and long for the LD reservoirs included in surv3 from the shapefiles for each reservoir.
# Read in here and merge with nid.lat.long
ldLatLong <- read_excel("ohio2016/inputData/ldLatLongForSurv3Reservoirs.xlsx")

survRes3latLong <- merge(survRes3latLong, ldLatLong, by = c("lake.name"), all = TRUE)

survRes3latLong <- mutate(survRes3latLong,
                          latitude = ifelse(is.na(latitude.x), latitude.y, latitude.x),
                          longitude = ifelse(is.na(longitude.x), longitude.y, longitude.x)) %>%
  select(-latitude.x, -latitude.y, -longitude.x, -longitude.y)

# Plot reservoirs
bbox <- make_bbox(longitude, latitude, data = survRes3latLong, f = 0.3)
map <- get_map(location = bbox, maptype = 'roadmap')
ggmap(map) + geom_point(data = survRes3latLong, 
                        aes(x = longitude, y = latitude,
                            color = luQuantBin,
                            shape = depthQuantBin),
                        size = 5) +
  scale_shape_discrete(name = "Depth",
                       labels = c("<26ft", "26-35ft", "35-52.5ft", "52.5-115ft")) +
  scale_color_discrete(name = "Ag land use",
                       labels = c("<27%", "27-42%", "42-62%", "63-86%"))

ggsave(filename="ohio2016/output/siteMap.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
