# SCRIPT TO PERFORM A QUICK PREVIEW OF LGR GHG DATA COLLECTED DURING THE 
# AUGUST 2014 SURVEY OF CH4 EMISSIONS FROM HARSHA LAKE.  ANALYZER WAS
# PROGRAMMED TO RECORD EVERY 20 SECONDS.

# LIBRARIES
library(ggplot2)
library(scales)


# List of .txt files containing data
txtFiles <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/data/greenhouseGasAnalyzer/", 
                       pattern="*.txt")

for (i in 1:length(txtFiles)) {  # loop to read and format each file
  gga.i <- read.table(paste("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/data/greenhouseGasAnalyzer/", 
                            txtFiles[i], sep=""),
                      sep=",",  # comma separate
                      skip=1,  # Skip first line of file.  Header info
                      as.is=TRUE, # Prevent conversion to factor
                      header=TRUE, # Import column names
                      fill=TRUE)  # Needed to deal with empty cells in last column
  #str(gga.i)
  #head(gga.i)
  
  # FORMAT DATA
  gga.i <- gga.i[1:(which(gga.i$Time == "-----BEGIN PGP MESSAGE-----") - 1), ]  # Remove PGP message
  gga.i$Time <- gsub("^\\s+|\\s+$", "", gga.i$Time)  #  Strip white spaces
  gga.i$Date <- substr(gga.i$Time, start=1, stop=10)  # Extract date
  gga.i$Second <- round(  # extract second, round to integer
    as.numeric(
      substr(gga.i$Time, start=nchar(gga.i$Time) - 5, stop=nchar(gga.i$Time))
    ), 
    digits=0)
  gga.i$Second <- ifelse(gga.i$Second == 60, 59, gga.i$Second)  # Chron can't handle 60 seconds
  gga.i$hms <- paste(substr(gga.i$Time, start=12, stop=17), gga.i$Second, sep="")  # time vector
  #   gga.i$RDateTime <- strptime(paste(gga.i$Date, gga.i$hms,sep=""),
  #                               format="%m/%d/%Y%H:%M:%S")  # POSIXlt object
  gga.i$RDateTime <- as.POSIXct(paste(gga.i$Date, gga.i$hms,sep=""),
                                format="%m/%d/%Y%H:%M:%S")  # POSIXct
  assign(paste("gga", i, sep = ""), gga.i)  # rename gga.i to gga1, gga2....
}  # End of loop

# Merge files
gga <- Reduce(function(...) merge(..., all=T), 
              list(gga1, gga2))  # Change if additional files added

# Format files
gga$Time <- gga$hms  # Remove hms
gga$RDate <- as.Date(gga$Date, format = "%m/%d/%Y")  # Remove Date
gga <- subset(gga, select = -c(hms, Date, Second))
names(gga)[grep("ppm", names(gga))] = gsub("^X.", "", names(gga)[grep("X", names(gga))])

# Add dome volume
gga$volume.L <- 19  # Need to update

# Basic plot
ggplot(gga, aes(RDateTime, CH4._ppm)) + geom_point() + 
  scale_x_datetime(labels=date_format ("%m/%d %H:%M"))

ggplot(gga, aes(RDateTime, CO2._ppm)) + geom_point() + 
  scale_x_datetime(labels=date_format ("%m/%d %H:%M"))

