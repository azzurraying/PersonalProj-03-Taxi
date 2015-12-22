library(maptools) # for getKMLcoordinate()
library(rgeos)
library(sp)
library(TTR) # for the function SMA()
library(quantmod) # for the function findPeaks()
#library(rsconnect)

shinyServer(function(input, output) {
    observe({
    # Get coordinates of Singapore's 55 planning areas.
    coords <- getKMLcoordinates(kmlfile = "Planning_Area_Census2010.kml",
                                ignoreAltitude=FALSE)
    # Clean data: remove last column
    for(i in 1:length(coords)) {
        coords[[i]] <- coords[[i]][, 1:2]
    }
    
    # Get corresponding names of Singapore's 55 planning areas
    areanames_reg <- read.table("SGP_planning_areas_reg.txt",
                            sep = "\t",
                            header = T)
    output$areas <- renderTable(areanames_reg)
    areanames <- as.character(areanames_reg$Neighborhood)
              
    # Read taxi data from the area of choice
    inputarea <- as.numeric(input$areacode)
    Samplearea <- read.table(paste("taxidatabytime", inputarea, ".txt", sep=''),
                             sep = "\t",
                             colClasses = c("Date", "POSIXct", "numeric", "factor", "factor"),
                             skip = 1)
    colnames(Samplearea) <- c("Date", "Time", "Taxi.avail", "Wday", "IsWday")
    # Smoothen data
    Samplearea.sm <- Samplearea
    Samplearea.sm$Taxi.avail <- SMA(Samplearea$Taxi.avail, n = input$range1)
    # Find peaks from smoothened data
    p <- findPeaks(Samplearea.sm$Taxi.avail, thresh = input$range2)
    
    ## Create map with user's input area highlighted on map, and a popup over it that describes the area.
    SPDF <- SpatialPointsDataFrame(coords=coords[[inputarea]], data = data.frame(coords[[inputarea]]))
    SP_centroid <- gCentroid(SPDF)

    # Output 1: leaflet map of SGP
    output$map <- renderLeaflet({
      leaflet() %>%
      addTiles() %>%
      addPolygons(data=coords[[inputarea]], weight=2) %>%
      addMarkers(as.numeric(data.frame(SP_centroid))[1],
                 as.numeric(data.frame(SP_centroid))[2],
                 popup = paste("Area ", inputarea, ": ", areanames[inputarea], sep='')) %>%
      addCircles(as.numeric(data.frame(SP_centroid))[1],
                 as.numeric(data.frame(SP_centroid))[2],
                 radius = max(Samplearea$Taxi.avail), weight = 1, color = "#777777",
                 fillOpacity = 0.7)
    })
    
    
    # Output 2: Plot the data with smoothened line fit and peak positions.
    output$taxiplot <- renderPlot({
        plot(Samplearea$Time,
             Samplearea$Taxi.avail,
             col = Samplearea$IsWday,
             pch = 20,
             xlab = "Date",
             ylab = paste("Number of taxis in ", areanames[inputarea], sep = ''))
        points(Samplearea.sm$Time,
               Samplearea.sm$Taxi.avail,
               type = "l",
               lwd = 4,
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
    })
    })
})

# c:/Users/jiangy
# Users/yingjiang
# library(shinyapps)
# shinyapps::deployApp('/Users/yingjiang/Dropbox/learnings/Stats_data/Projects/taxi_data/Taxi')

