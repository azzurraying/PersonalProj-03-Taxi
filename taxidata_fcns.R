### TASK 1: Get coordinates of 55 urban planning areas, figure out what each of these area is.
# Takes in:
# 1. A kml file containing a list of 55 planning areas' coordinates
# 2. A text file listing the names of 55 planning areas
# Reads the 55 sets of coordinates and combines with the 55 names into a list of 55 dataframes.
# Returns the list of 55 dataframes
# E.g. areacoords
getarea <- function(kml = "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Planning_areas/Planning_Area_Census2010.kml",
                    areanames = "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Planning_areas/SGP_planning_areas.txt") {
    # Get a list of 55 elements; each element is an area polygon (identity unknown)
    # The area polygon has length(coords[[i]]) vertices
    # E.g. input arguments:
    # kml = "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Planning_areas/Planning_Area_Census2010.kml"
    # areanames = "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Planning_areas/SGP_planning_areas.txt"
    library(maptools)
    coords <- getKMLcoordinates(kmlfile = kml,
                                ignoreAltitude=FALSE)
    # Get area names. Note: length(areanames) is the same as length(coords)
    areanames1 <- readLines(areanames)
    # Convert data of each polygon into a dataframe.
    # Add the area name.
    for(i in 1:length(coords)) {
        coords[[i]] <- data.frame(coords[[i]])
        coords[[i]]$Area <- rep(areanames1[i], nrow(coords[[i]]))
    }
    return(coords)
}
# Execution code:
areacoords <- getarea(kml = "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Planning_areas/Planning_Area_Census2010.kml",
                      areanames = "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Planning_areas/SGP_planning_areas.txt")
setwd("/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Dataframes")
write(areacoords, file = "areacoords")

### TASK 3: Get and clean taxi data
# Takes in the main data directory.
# Goes through each subdirectory and read the data within.
# Returns the final dataframe with all taxi coordinates.
# E.g. taxicoords
gettaxi <- function(maindir = "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Data") {
    ## Read in JSON data
    library("jsonlite")
    library(httr)
    taxidata <- list()
    filedirs <- list.dirs(maindir)
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
    return(taxidata2)
}
# Execution code:
taxicoords <- gettaxi(maindir = "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Data")
setwd("/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Dataframes")
write.csv(taxicoords, file = "taxicoords.csv")


### Task 3: Find out area to which each taxi data point belongs
# Takes in a dataframe of taxi coordinates, and a list of planning area coordinates.
# Evaluates whether the coordinates of each taxi datapoint is within each of the area polygons.
# Returns the taxi coordinates with a new column indicating which area each data point belongs to.
# E.g. taxidata
gettaxireg <- function(taxicoords = taxicoords,
                        areacoords = areacoords) {
    library(mgcv)    
    # Make numerical matrices out of both taxi coordinates and area polygon coordinates
    
    # Exchange the Latitude and Longitude columns of taxicoords.
    taxicoords1 <- as.matrix(taxicoords[, 2:1])
    # Take only the Latitude and Longitude columns of areacoords
    areacoords1 <- list()
    for(i in 1:length(areacoords)) {
        areacoords1[[i]] <- as.matrix(areacoords[[i]][, 1:2])
    }
    
    # Go through all rows of taxi data, see if each belongs to which area polygon.
    inside <- list()
    taxicoords$Area <- character(nrow(taxicoords))
    for(i in 1:length(areacoords1)) {
        # Loop through each area. E.g. inside[[1]] cooresponds to all taxis within Pasir Ris.
        inside[[i]] <- in.out(areacoords1[[i]], taxicoords1)
        taxicoords$Area[inside[[i]]] <- areacoords[[i]]$Area[1] # It's a vector of identical characters. Just take the 1st one.
    }
    return(taxicoords)
}
# Execution code:
taxidata <- gettaxireg(taxicoords = taxicoords,
                       areacoords = areacoords)
write.table(taxidata, "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Dataframes/taxidata_all.txt") # Might be slow.



### Task 4: Get taxi data of a specific area

# Takes in:
# 1. taxidata that has the following colnames:
# "Latitude"  "Longitude" "Date"      "Time"      "Date.Time" "Area"
# 2. The index of the desired sample planning area. E.g. 19 cooresponds to "Clementi". 46 to "Downtown Core".
# Converts the Time into POSIXct format. Orders the dataframe by time. Creates a Wkday column.
# Returns the cleaned taxidata for the desired sample area.
# E.g. Clementi (for areaindex = 19)

taxibyarea <- function(data = taxidata,
                       areanames = "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Planning_areas/SGP_planning_areas.txt",
                       areaindex) {
    
    areanames <- readLines(areanames)
    # Get desired area out of taxidata.
    Samplearea <- taxidata[which(taxidata$Area == areanames[areaindex]), ]
    # Change time into proper format. (Set Date.Time.) This is the slowest step
    Samplearea$Date.Time <- as.POSIXct(Samplearea$Date)
    Samplearea$Wday <- wday(Samplearea$Date, label = T)
    # Create IsWday column: States whether each day is a wkday or wkend. Defaut = wkday.
    Samplearea$IsWday <- "Weekday"
    return(Samplearea)
}

### Task 4a: Get taxi data of all areas; write them to file

# Takes in:
# 1. taxidata that has the following colnames:
# "Latitude"  "Longitude" "Date"      "Time"      "Date.Time" "Area"
# 2. The list of areanames
# Attaches Date.Time, Wday, IsWday columns.
# Writes the new dataframes to file.

taxibyarea1 <- function(data = taxidata,
                        areanames = "/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Planning_areas/SGP_planning_areas.txt") {
    
    areanames <- readLines(areanames)
    for(i in 27:length(areanames)) {
        # Get desired area out of taxidata.
        # Note: Area 26 (North-Eastern Islands) is empty. Writing this file will give the error: 
        # Error in `$<-.data.frame`(`*tmp*`, "IsWday", value = "Weekday") : 
        # replacement has 1 row, data has 0
        Samplearea <- taxidata[which(taxidata$Area == areanames[i]), ]
        # Create Date.Time, Wday, IsWday columns
        Samplearea$Date.Time <- as.POSIXct(Samplearea$Date)
        Samplearea$Wday <- wday(Samplearea$Date, label = T)
        Samplearea$IsWday <- "Weekday"
        write.table(Samplearea,
                    paste("/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Dataframes/taxidata", i, ".txt", sep = ''),
                    row.names = F,
                    col.names = colnames(Samplearea),
                    sep = "\t")
    }
}


### Task 5: Format time

timeformat <- function(areaindex) {
    data <- read.table(paste("/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Dataframes/taxidata", areaindex, ".txt", sep = ''),
                       sep = "\t",
                       colClasses = c("numeric", "numeric", "Date", "character", "character", "POSIXct", "factor", "factor"),
                       skip = 1)
    colnames(data) <- c("Latitude", "Longitude", "Date", "Time", "Area", "Date.Time", "Wday", "IsWday")
    #print(head(data))
    data$Date.Time <- ymd_hms(paste(data$Date, data$Time))
#     for(i in 1:nrow(data)) {
#         data$Date.Time[i] <- ymd_hms(paste(data$Date[i], data$Time[i]))
#         print(i)
#         print(data$Date.Time[i])
#     }
    return(data)
}

### Task 5a: Format time in chunks, with records of data.
# Queenstown: E.g. 341194/7
timeformat1 <- function(areaindex, chunks) {
    data <- read.table(paste("/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Dataframes/taxidata", areaindex, ".txt", sep = ''),
                       sep = "\t",
                       skip = 1,
                       stringsAsFactors = F)
    dataspl <- split(data, ceiling(seq_along(data)/(nrow(data) / chunks)))
    colnames(data) <- c("Latitude", "Longitude", "Date", "Time", "Area", "Date.Time", "Wday", "IsWday")
    for(i in 1:nrow(data)) {
        data$Date.Time[i] <- ymd_hms(paste(data$Date[i], data$Time[i]))
    }
}

### Task 5b: Format time, create new dataframe with total number of taxis in the area of interest across times of the day.
# Takes in:
# 1. A dataframe of taxi coordinates in a planning area of interest (e.g. Clementi) over the entire time duration measured. The time is already correctly formatted.
# 2. An upper tolerance number of taxis, above which data points measured at these times are considered outliers. E.g. 1000.
# Computes the total number of taxis at a given time.
# Returns
timeformat <- function(areaindex, tol) {
    data <- read.table(paste("/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Dataframes/taxidata", areaindex, ".txt", sep = ''),
                       sep = "\t",
                       colClasses = c("numeric", "numeric", "Date", "character", "character", "POSIXct", "factor", "factor"),
                       skip = 1)
    colnames(data) <- c("Latitude", "Longitude", "Date", "Time", "Area", "Date.Time", "Wday", "IsWday")
    # Format time column properly.
    data$Date.Time <- ymd_hms(paste(data$Date, data$Time))
    # Order data by date and time.
    data <- data[order(data$Date.Time), ]
    
    # Find total number of taxi at a given datetime
    taxiavail <- numeric(length(unique(data$Date.Time)))
    for(i in 1:length(unique(data$Date.Time))) {
        taxiavail[i] <- sum(data$Date.Time == unique(data$Date.Time)[i])
    }
    
    # Check for outlier:
    if(sum(taxiavail > tol) > 0) {
        outlierindex <- which(taxiavail > tol)
        for(i in 1:length(outlierindex)) {
            taxiavail[outlierindex[i]] <- 
                ave(taxiavail[outlierindex[i]-1], taxiavail[outlierindex[i]+1])            
        }
    }
    # E.g. for Clementi, remove an outlier. taxiSamplearea[427] is over 3000. Why???
    
    # Create new data.frame.
    taxiSamplearea <- data.frame(Date = as.Date(substr(unique(data$Date.Time), 1, 10)),
                                 Time = unique(data$Date.Time),
                                 Taxi.avail = taxiavail,
                                 Wday = wday(unique(data$Date.Time), label = T),
                                 IsWday = "Weekday",
                                 stringsAsFactors = F)
    taxiSamplearea$IsWday[grep("Sat|Sun", taxiSamplearea$Wday)] <- "Weekend"
    taxiSamplearea$IsWday <- as.factor(taxiSamplearea$IsWday)
    return(list(data, taxiSamplearea))
}
# Execution code:
for(i in 1:length(areanames)) {
    Samplearea <- timeformat(i, 1000)
    write.table(Samplearea[[2]], paste(areanames[i], "overtime.txt", sep = ""))
}
### IGNORE THE ABOVE TASK 5 ###

### TASK 5 (good):
# Takes in:
# 1. A dataframe of taxi coordinates in a UPA of interest (e.g. Clementi) over the entire time duration measured. The time is already correctly formatted.
# 2. An upper tolerance number of taxis, above which data points measured at these times are considered outliers. E.g. 1000.
# Computes the total number of taxis at a given time.
# Returns the available taxis over time for a given area.
taxiavail <- function(areaindex, tol) {
    # Reads in data corresponding to taxis from a UPA of interest.
    data <- read.table(paste("/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Dataframes/taxidatabyarea", areaindex, ".txt", sep = ''),
                       sep = "\t",
                       colClasses = c("numeric", "numeric", "Date", "character", "character", "POSIXct", "factor", "factor"),
                       skip = 1)
    colnames(data) <- c("Latitude", "Longitude", "Date", "Time", "Area", "Date.Time", "Wday", "IsWday")
    
    # Find total number of taxi at a given date and time.
    taxiavail <- numeric(length(unique(data$Date.Time)))
    for(i in 1:length(unique(data$Date.Time))) {
        taxiavail[i] <- sum(data$Date.Time == unique(data$Date.Time)[i])
    }
    
    # Check for outliers.
    if(sum(taxiavail > tol) > 0) {
        outlierindex <- which(taxiavail > tol)
        for(i in 1:length(outlierindex)) {
            taxiavail[outlierindex[i]] <- 
                ave(taxiavail[outlierindex[i]-1], taxiavail[outlierindex[i]+1])            
        }
    }
    # E.g. for Clementi, remove an outlier. taxiSamplearea[427] is over 3000.
    
    # Create new data.frame.
    taxiSamplearea <- data.frame(Date = as.Date(substr(unique(data$Date.Time), 1, 10)),
                                 Time = unique(data$Date.Time),
                                 Taxi.avail = taxiavail,
                                 Wday = wday(unique(data$Date.Time), label = T),
                                 IsWday = "Weekday",
                                 stringsAsFactors = F)
    # Modify the "weekday" columns.
    taxiSamplearea$IsWday[grep("Sat|Sun", taxiSamplearea$Wday)] <- "Weekend"
    taxiSamplearea$IsWday <- as.factor(taxiSamplearea$IsWday)
    
    return(taxiSamplearea)
}
# Execution code: write all areas to file.
for(i in 27:55) {
    Samplearea <- taxiavail(i, 1000)
    write.table(Samplearea,
                paste("/Users/yingjiang/Dropbox/Learnings/Stats_data/Projects/taxi_data/Dataframes/taxidatabytime", i, ".txt", sep = ""),
                row.names = F,
                col.names = colnames(Samplearea),
                sep = "\t")
}

# test and see if the reading is done right.
# execute the last 2 chunks of code.


### Task 6: Preliminary plot

prelimplot <- function(Samplearea) {
    library(ggplot2)
    qplot(Time,
          Taxi.avail,
          colour=IsWday,
          data=Samplearea)
}
# Execution code:
prelimplot(Mandai1[[2]])



### Task 7: Find peaks and valleys. Final plot

taxiplot <- function(Samplearea, # Dataframe of an area, with Date, time, taxiavailable, Weekday info. E.g. Clementi1[[2]]
                     areaindex, # Area index corresponding to the Samplearea
                     n = 50, # parameter for data smoothing. Larger => average over more data points => smoother
                     thresh = 0.1) { # parameter for peakfinding threshold. Lower => more peaks found
    library(TTR) # for the function SMA()
    library(quantmod) # for the function findPeaks()
    
    # Smoothen data
    Samplearea.sm <- Samplearea
    Samplearea.sm$Taxi.avail <- SMA(Samplearea$Taxi.avail, n = n)
    # Find peaks from smoothened data
    p <- findPeaks(Samplearea.sm$Taxi.avail, thresh = thresh)
    
    # Plot the fluctuations.
    quartzFonts(avenir = c("Avenir Book",
                           "Avenir Black",
                           "Avenir Book Oblique",
                           "Avenir Black Oblique"))
    par(bg = "mintcream",
        family = "avenir")
    palette(c("yellowgreen", "lightgoldenrod"))
    plot(Samplearea$Time,
         Samplearea$Taxi.avail,
         col = Samplearea$IsWday,
         xlab = "Date",
         ylab = paste("Number of taxis in ", areanames[areaindex], sep = ''))
    points(Samplearea.sm$Time,
           Samplearea.sm$Taxi.avail,
           type = "l",
           lwd = 5,
           col = "green4")
    points(Samplearea.sm$Time[p],
           Samplearea.sm$Taxi.avail[p],
           pch = 19,
           col = "orchid")
    abline(h = ave(Samplearea.sm$Taxi.avail[p])[1])
    legend("topright",
           legend = levels(Samplearea$IsWday),
           col = 1:length(Samplearea$IsWday),
           pch = 1)
    
    # Find positions of calculated peaks and valleys
    # Create subsets that correspond to the taxi situation at calculated peak and valley times respectively.
    Calculatedpeaks <- Samplearea[p, ][which(Samplearea.sm$Taxi.avail[p] > ave(Samplearea.sm$Taxi.avail[p])), ]
    Calculatedvalleys <- Samplearea[p, ][which(Samplearea.sm$Taxi.avail[p] < ave(Samplearea.sm$Taxi.avail[p])), ]
    
    # These calculated peak and valley times lag behind the real ones.
    # Figure out what this lag is for each day.
    dailymax <- numeric()
    dailypeaktime <- list()
    dailycalcmax <- numeric()
    dailycalcpeaktime <- list()
    
    dailymin <- numeric()
    dailyvalleytime <- list()
    dailycalcmin <- numeric()
    dailycalcvalleytime <- list()
    for(i in 1:length(unique(Samplearea$Date))) {
        dateind <- unique(Samplearea$Date)[i]
        
        dailymax[i] <- max(Samplearea$Taxi.avail[Samplearea$Date == dateind])
        dailypeaktime[[i]] <- ave(Samplearea$Time[Samplearea$Date == dateind][Samplearea$Taxi.avail[Samplearea$Date == dateind] == dailymax[i]])[1]
        dailycalcmax[i] <- max(Calculatedpeaks$Taxi.avail[Calculatedpeaks$Date == dateind])
        dailycalcpeaktime[[i]] <- ave(Calculatedpeaks$Time[Calculatedpeaks$Date == dateind][Calculatedpeaks$Taxi.avail[Calculatedpeaks$Date == dateind] == dailycalcmax[i]])[1]
        
        dailymin[i] <- min(Samplearea$Taxi.avail[Samplearea$Date == dateind])
        dailyvalleytime[[i]] <- ave(Samplearea$Time[Samplearea$Date == dateind][Samplearea$Taxi.avail[Samplearea$Date == dateind] == dailymin[i]])[1]
        dailycalcmin[i] <- min(Calculatedvalleys$Taxi.avail[Calculatedvalleys$Date == dateind])
        dailycalcvalleytime[[i]] <- ave(Calculatedvalleys$Time[Calculatedvalleys$Date == dateind][Calculatedvalleys$Taxi.avail[Calculatedvalleys$Date == dateind] == dailycalcmin[i]])[1]
        
    }
    
    Peakdiff <- list()
    Valleydiff <- list()
    for(i in 1:length(unique(Samplearea$Date))) {
        Peakdiff[[i]] <- dailycalcpeaktime[[i]][1] - dailypeaktime[[i]][1]
        Valleydiff[[i]] <- dailycalcvalleytime[[i]][1] - dailyvalleytime[[i]][1]
    }
    
    dailypeakvalley <- data.frame(Date = unique(Samplearea$Date),
                                  Peak.taxi.no = dailymax,
                                  Peak.time = do.call(c, dailypeaktime),
                                  Peak.time.calc = do.call(c, dailycalcpeaktime),
                                  Valley.taxi.no = dailymin,
                                  Valley.time = do.call(c, dailyvalleytime),
                                  Valley.time.calc = do.call(c, dailycalcvalleytime))
    return(dailypeakvalley)
}

plot(x = Clementidaily$Date,
     y = unlist(strsplit(as.character(Clementidaily$Peak.time), ' '))[seq(2, 16, 2)])
plot(x = Clementidaily$Date,
     y = ts(Clementidaily$Peak.time))

plot(x = Clementidaily$Date,
     y = Clementidaily$Peak.time)

points(x = Clementidaily$Date,
       y = Clementidaily$Peak.time.calc,
       pch = 19)
points(x = Downtowndaily$Date,
       y = Downtowndaily$Peak.time,
       col = "red")
points(x = Downtowndaily$Date,
       y = Downtowndaily$Peak.time.calc,
       col = "red",
       pch = 19)

# Or:
y1 <- ts(Clementidaily$Peak.time)
for (i in 3:length(y1)) {
    y1[i] <- y1[i] - 86400*(i - 2)
}
#y1a <- y1 - 1435000000

y2 <- ts(Clementidaily$Peak.time.calc)
for (i in 3:length(y2)) {
    y2[i] <- y1[i] - 86400*(i - 2)
}
#y2a <- y2 - 1435000000

y3 <- ts(Downtowndaily$Peak.time)
for (i in 3:(length(y3)-1)) {
    y3[i] <- y1[i] - 86400*(i - 2)
}
y3[length(y3)] <- y3[length(y3)] - 86400*(length(y3) - 1)
#y3a <- y3 - 1435000000

y4 <- ts(Downtowndaily$Peak.time.calc)
for (i in 3:(length(y4)-1)) {
    y4[i] <- y1[i] - 86400*(i - 2)
}
y4[length(y4)] <- y4[length(y4)] - 86400*(length(y4) - 1)
#y4a <- y4 - 1435000000

plot(x = Clementidaily$Date,
     y = y1,
     ylim = c(1435600000, 1436400000),
     xlab = "Date",
     ylab = "Daily peak times in time-series data",
     pch = 19)
points(x = Clementidaily$Date,
       y = y2,
       col = "grey",
       pch = 4)
points(x = Downtowndaily$Date,
       y = y3,
       col = "red",
       pch = 19)
points(x = Downtowndaily$Date,
       y = y4,
       col = "pink",
       pch = 4)
legend("bottomleft",
       legend = c("Clementi peak time", "Clementi peak time calculated", "Downtown peak time", "Downtown peak time calculated"),
       col = c("black", "grey", "red", "pink"),
       pch = c(19,4,19,4))
