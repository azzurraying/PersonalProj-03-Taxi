### TASK 1: Get coordinates
library(maptools)
# Get coordinates of 55 planning areas
# Get a list of 55 elements; each element is an area (identity unknown)
# The area is a polygon with length(coords[[i]]) vertices
coords <- getKMLcoordinates(kmlfile = "/Users/yingjiang/Desktop/taxi_data/Planning_areas/Planning_Area_Census2010.kml",
                            ignoreAltitude=FALSE)

### TASK 2: Try to figure out which area does each list element (polygon) correspond to. Generate planning area polygons.
areanames <- readLines("/Users/yingjiang/Desktop/taxi_data/Planning_areas/SGP_planning_areas.txt")
coordsdf <- list()
for(i in 1:length(coords)) {
    coordsdf[[i]] <- data.frame(coords[[i]])
    coordsdf[[i]]$Area <- rep(areanames[i], nrow(coords[[i]]))
}
# We get a list of 55 dataframes


### TASK 3: Get and clean taxi data

## Read in JSON data
library("jsonlite")
library(httr)

taxidata <- list()
filedirs <- list.dirs("/Users/yingjiang/Desktop/taxi_data/Data")
for(i in 2:length(filedirs)) {
    # Go into each directory
    setwd(filedirs[i])
    # Set date
    temp <- strsplit(getwd(), "/")[[1]]
    date <- temp[length(temp)]
    files <- list.files()
    taxidata[[i]] <- list()
    for(j in 1:length(files)) {
        # Read in JSON
        taxidata[[i]][[j]] <- fromJSON(files[j])
        # append date label
        taxidata[[i]][[j]]$Date <- date
        # append time label
        taxidata[[i]][[j]]$Time <- files[j]
    }
}

## Combine list elements into 1 single dataframe.
taxidata1 <- list()
for(i in 2:length(taxidata)) {
    taxidata1[[i]] <- do.call("rbind", taxidata[[i]])
}
# It's also ok to use do.call without parenthesis on the function rbind:
taxidata2 <- do.call(rbind, taxidata1)

## Fix Date and Time formats (Subtask 2 and 3 are better done after taxidata is split into different areas!)
library(lubridate)

# Subtask 1: Convert Date into POSIXct format
taxidata2$Date <- as.Date(taxidata2$Date)

##############
# OPTIONAL BELOW!
# Subtask 2: Determine if each day is weekday or weekend
# This section is better worked out for datasets of individual areas!
taxidata2$Wday <- wday(taxidata2$Date, label = T)
taxidata2$IsWday <- character(nrow(taxidata2))
for(i in 1:nrow(taxidata2)) {
    if(taxidata2$Wday[i] == "Sat" | taxidata2$Wday[i] == "Sun") {
        taxidata2$IsWday[i] <- "Weekday"
    }
    else {
        taxidata2$IsWday[i] <- "Weekend"
    }
}

# Subtask 3: Convert Time into POSIXct format (BOTH DON'T WORK!!)
# This section is better worked out for datasets of individual areas!
# Method1
taxidata2$Date.Time <- as.POSIXct(taxidata2$Date)
for(i in 1:nrow(taxidata2)) {
    taxidata2$Date.Time[i] <- ymd_hms(paste(taxidata2$Date[i], taxidata2$Time[i]))
}
# Method2 (create new vector, one more step of stiching it on)
a1 <- as.POSIXct(taxidata2$Date)
for(i in 1:nrow(taxidata2)) {
    a1[i] <- ymd_hms(paste(taxidata2$Date[i], taxidata2$Time[i]), tz = "UTC")
}
taxidata2$Date.Time2 <- a1
# This algorithm doesn't work with the entire dataset!
# But worked on a smaller dataset, e.g. the Clementi subset.
# Unclean data in the big set? Unprocessable due to size?
##############


### TASK 4: Find out area to which each taxi data point belongs

library(mgcv)

# This is a list of coordinates of all taxis within the dates and times for which data is available.
# Make matrices out of both taxi coordinates and area polygon coordinates
coordstaxi <- as.matrix(taxidata2[, 2:1])
coordsarea <- list()
for(i in 1:length(coordsdf)) {
    coordsarea[[i]] <- as.matrix(coordsdf[[i]][, 1:2])
}

# Find out if the taxi coordinates belong to each of the polygons that delineates Singapore's urban planning regions
# A simple test
for(i in 1:length(coordsarea)) {
    print(in.out(coordsarea[[i]], coordstaxi[1, ]))
}
# One of them is TRUE. So this taxi datapoint belongs one of the areas.

# Go through all rows of taxi data, see if each belongs to which area polygon.
inside <- list()
taxidata2$Area <- character(nrow(taxidata2))
for(i in 1:length(coordsarea)) {
    # Loop through each area. E.g. inside[[1]] cooresponds to all taxis within Pasir Ris.
    inside[[i]] <- in.out(coordsarea[[i]], coordstaxi)
    taxidata2$Area[inside[[i]]] <- coordsdf[[i]]$Area[1] # It's a vector of identical characters. Just take the 1st one.
}

# Quick check for total number of taxis in each area
for(i in 1:length(inside)) {
    print(sum(inside[[i]]))
}



### TASK 5: Explore the taxidata

# We have a dataframe that looks like:
# Latitude  Longitude   Date    Time    Date.Time(wrong)    Area
# Taxi_lat  Taxi_long   Date    Time    Time(wrong)         Planning_area ...

# Want to find out:
# 1. Over 1 weekday day, how does the taxi availability in each area rise and fall? (Choose a CBD and a residence region e.g. Clementi)
# 3. Over 1 week (and on each individual day of the week), how does the taxi availability in each area rise and fall?
# 4. Over 1 weekday day, when's the peak/lowest availability in each area?
# 5. Over 1 weekend day, when's the peak/lowest availability in each area?
# 6. At a certain time (e.g. peak time), what's the availability across all areas? Where's highest?

###################################################################################
# Split taxidata by area
# Take Clementi, for e.g.
# Change time into proper format.
Clementi <- taxidata2[which(taxidata2$Area == areanames[19]), ]
for(i in 1:nrow(Clementi)) {
    Clementi$Date.Time[i] <- ymd_hms(paste(Clementi$Date[i], Clementi$Time[i]))
}
length(unique(Clementi$Date.Time)) # > 2000
# Order Clementi data by date and time.
Clementi1 <- Clementi[order(Clementi$Date.Time), ]
# Determine day-of-week for each data point
Clementi1$Wday <- wday(Clementi1$Date, label = T)
# Determine if each day is a weekday or weekend
Clementi1$IsWday <- "Weekday"
Clementi1$IsWday[grep("Sat|Sun", Clementi1$Wday)] <- "Weekend"
Clementi1$IsWday <- as.factor(Clementi1$IsWday)

# Split taxidata by datetime (find total number of taxi in Clementi at a given datetime)
taxiavail.Clementi <- numeric(length(unique(Clementi1$Date.Time)))
for(i in 1:length(unique(Clementi1$Date.Time))) {
    taxiavail.Clementi[i] <- sum(Clementi1$Date.Time == unique(Clementi1$Date.Time)[i])
}
# Removed an outlier:
# taxiClementi[427] is over 3000. Why???
taxiavail.Clementi[427] <- ave(taxiavail.Clementi[426], taxiavail.Clementi[428])


# Plots:
taxiClementi <- data.frame(Time = unique(Clementi1$Date.Time),
                           Taxi.avail = taxiavail.Clementi,
                           Wday = wday(unique(Clementi1$Date.Time), label = T),
                           IsWday = "Weekday")
taxiClementi$IsWday <- "Weekday"
taxiClementi$IsWday[grep("Sat|Sun", taxiClementi$Wday)] <- "Weekend"
taxiClementi$IsWday <- as.factor(taxiClementi$IsWday)

plot(Time, Taxi.avail,
     data = taxiClementi,
     type = "l",
     col = IsWday)

library(ggplot2)
qplot(Time,
      Taxi.avail,
      colour=IsWday,
      data=taxiClementi)
################################################################################

###################################################################################
### Split taxidata by area
# Take Clementi, for e.g.
Clementi <- taxidata2[which(taxidata2$Area == areanames[19]), ]

### Change time into proper format.
for(i in 1:nrow(Clementi)) {
    Clementi$Date.Time[i] <- ymd_hms(paste(Clementi$Date[i], Clementi$Time[i]))
}
length(unique(Clementi$Date.Time)) # > 2000
# Order Clementi data by date and time.
Clementi1 <- Clementi[order(Clementi$Date.Time), ]
# Determine day-of-week for each data point
Clementi1$Wday <- wday(Clementi1$Date, label = T)
# Determine if each day is a weekday or weekend
Clementi1$IsWday <- "Weekday"
Clementi1$IsWday[grep("Sat|Sun", Clementi1$Wday)] <- "Weekend"
Clementi1$IsWday <- as.factor(Clementi1$IsWday)

# Split taxidata by datetime (find total number of taxi in Clementi at a given datetime)
taxiavail.Clementi <- numeric(length(unique(Clementi1$Date.Time)))
for(i in 1:length(unique(Clementi1$Date.Time))) {
    taxiavail.Clementi[i] <- sum(Clementi1$Date.Time == unique(Clementi1$Date.Time)[i])
}
# Check for outlier:
sum(taxiavail.Clementi > 1000)
# Removed an outlier:
# taxiClementi[427] is over 3000. Why???
taxiavail.Clementi[427] <- ave(taxiavail.Clementi[426], taxiavail.Clementi[428])


### Preliminary plot
taxiClementi <- data.frame(Time = unique(Clementi1$Date.Time),
                           Taxi.avail = taxiavail.Clementi,
                           Wday = wday(unique(Clementi1$Date.Time), label = T),
                           IsWday = "Weekday")
taxiClementi$IsWday <- "Weekday"

library(ggplot2)
qplot(Time,
      Taxi.avail,
      colour=IsWday,
      data=taxiClementi)

### Find peaks and valleys
# loess (polynomial smoothing) doesn't work
# taxiClementi.sm <- loess(taxiClementi$Taxi.avail ~ ts(taxiClementi$Time))$fitted
library(TTR) # for the function SMA()
# Note, higher n => smoother curve. Doesn't really matter from 10 - 20...
plot(taxiClementi$Time,
       taxiClementi$Taxi.avail,
       col = "red")
points(taxiClementi$Time,
     SMA(taxiClementi$Taxi.avail, n = 20),
     type = 'l',
     lwd = 2)

taxiClementi.sm.df <- taxiClementi
taxiClementi.sm.df$Taxi.avail <- SMA(taxiClementi$Taxi.avail, n = 50)

library(quantmod) # for the function findPeaks()
p <- findPeaks(taxiClementi.sm.df$Taxi.avail, thresh = 0.1)

quartzFonts(avenir = c("Avenir Book",
                       "Avenir Black",
                       "Avenir Book Oblique",
                       "Avenir Black Oblique"))
par(bg = "mintcream",
    family = "avenir")
palette(c("yellowgreen", "lightgoldenrod"))
plot(taxiClementi$Time,
     taxiClementi$Taxi.avail,
     col = taxiClementi$IsWday,
     xlab = "Date",
     ylab = paste("Number of taxis in ", areanames[19], sep = ''))
points(taxiClementi.sm.df$Time,
     taxiClementi.sm.df$Taxi.avail,
     type = "l",
     lwd = 5,
     col = "green4")
points(taxiClementi.sm.df$Time[p],
       taxiClementi.sm.df$Taxi.avail[p],
       pch = 19,
       col = "orchid")
# abline(h = ave(c(max(taxiClementi$Taxi.avail), min(taxiClementi$Taxi.avail))))
abline(h = ave(taxiClementi.sm.df$Taxi.avail[p])[1])
legend("topright",
       legend = levels(taxiClementi$IsWday),
       col = 1:length(taxiClementi$IsWday),
       pch = 1)

peaks <- which(taxiClementi.sm.df$Taxi.avail[p] > ave(taxiClementi.sm.df$Taxi.avail[p]))
valleys <- which(taxiClementi.sm.df$Taxi.avail[p] < ave(taxiClementi.sm.df$Taxi.avail[p]))
taxiClementi$Time[p][peaks]
taxiClementi$Time[p][valleys]

## More complicated stuff below:
diff <- numeric(nrow(taxiClementi))
for(i in 2:length(diff)) {
    diff[i] <- (taxiClementi.sm.df$Taxi.avail[i+1] - taxiClementi.sm.df$Taxi.avail[i]) / ts(taxiClementi.sm.df$Time[i+1] - taxiClementi.sm.df$Time[i])
}
# Below shows the number of consecutive signs of slopes, using rle().
# E.g., before a maxima, there should be a LONG series of consecutive + signs for slopes.
# After a maxima, there should be a LONG series of consecutive - signs.
# However, some of the series are short: 4, 6, 3, 1...
# This means frequent flips in signs.
# This means there are lots of local maxima and minima => noisy data!
rle(sign(diff))$length

################################################################################

###################################################################################
#### Do a downtown area:

### Split taxidata by area
# Take "Downtown Core", for e.g.
Downtown <- taxidata2[which(taxidata2$Area == "Downtown Core"), ]

### Change time into proper format.
for(i in 1:nrow(Downtown)) {
    Downtown$Date.Time[i] <- ymd_hms(paste(Downtown$Date[i], Downtown$Time[i]))
}
length(unique(Downtown$Date.Time)) # > 2000
# Order Downtown data by date and time.
Downtown1 <- Downtown[order(Downtown$Date.Time), ]
# Determine day-of-week for each data point
Downtown1$Wday <- wday(Downtown1$Date, label = T)
# Determine if each day is a weekday or weekend
Downtown1$IsWday <- "Weekday"
Downtown1$IsWday[grep("Sat|Sun", Downtown1$Wday)] <- "Weekend"
Downtown1$IsWday <- as.factor(Downtown1$IsWday)

# Split taxidata by datetime (find total number of taxi in Downtown at a given datetime)
taxiavail.Downtown <- numeric(length(unique(Downtown1$Date.Time)))
for(i in 1:length(unique(Downtown1$Date.Time))) {
    taxiavail.Downtown[i] <- sum(Downtown1$Date.Time == unique(Downtown1$Date.Time)[i])
}
# Check for outlier:
sum(taxiavail.Downtown > 1000)
# Removed an outlier:
# taxiDowntown[427] is over 3000. Why???
taxiavail.Downtown[427] <- ave(taxiavail.Downtown[426], taxiavail.Downtown[428])


### Preliminary plot
taxiDowntown <- data.frame(Time = unique(Downtown1$Date.Time),
                           Taxi.avail = taxiavail.Downtown,
                           Wday = wday(unique(Downtown1$Date.Time), label = T),
                           IsWday = "Weekday")
taxiDowntown$IsWday <- "Weekday"

library(ggplot2)
qplot(Time,
      Taxi.avail,
      colour=IsWday,
      data=taxiDowntown)

### Find peaks and valleys
# loess (polynomial smoothing) doesn't work
# taxiDowntown.sm <- loess(taxiDowntown$Taxi.avail ~ ts(taxiDowntown$Time))$fitted
library(TTR) # for the function SMA()
# Note, higher n => smoother curve. Doesn't really matter from 10 - 20...
plot(taxiDowntown$Time,
     taxiDowntown$Taxi.avail,
     col = "red")
points(taxiDowntown$Time,
       SMA(taxiDowntown$Taxi.avail, n = 20),
       type = 'l',
       lwd = 2)

taxiDowntown.sm.df <- taxiDowntown
taxiDowntown.sm.df$Taxi.avail <- SMA(taxiDowntown$Taxi.avail, n = 50)

library(quantmod) # for the function findPeaks()
p <- findPeaks(taxiDowntown.sm.df$Taxi.avail, thresh = 0.1)

quartzFonts(avenir = c("Avenir Book",
                       "Avenir Black",
                       "Avenir Book Oblique",
                       "Avenir Black Oblique"))
par(bg = "mintcream",
    family = "avenir")
palette(c("yellowgreen", "lightgoldenrod"))
plot(taxiDowntown$Time,
     taxiDowntown$Taxi.avail,
     col = taxiDowntown$IsWday,
     xlab = "Date",
     ylab = paste("Number of taxis in ", areanames[19], sep = ''))
points(taxiDowntown.sm.df$Time,
       taxiDowntown.sm.df$Taxi.avail,
       type = "l",
       lwd = 5,
       col = "green4")
points(taxiDowntown.sm.df$Time[p],
       taxiDowntown.sm.df$Taxi.avail[p],
       pch = 19,
       col = "orchid")
# abline(h = ave(c(max(taxiDowntown$Taxi.avail), min(taxiDowntown$Taxi.avail))))
abline(h = ave(taxiDowntown.sm.df$Taxi.avail[p])[1])
legend("topright",
       legend = levels(taxiDowntown$IsWday),
       col = 1:length(taxiDowntown$IsWday),
       pch = 1)

peaks <- which(taxiDowntown.sm.df$Taxi.avail[p] > ave(taxiDowntown.sm.df$Taxi.avail[p]))
valleys <- which(taxiDowntown.sm.df$Taxi.avail[p] < ave(taxiDowntown.sm.df$Taxi.avail[p]))
taxiDowntown$Time[p][peaks]
taxiDowntown$Time[p][valleys]

################################################################################
