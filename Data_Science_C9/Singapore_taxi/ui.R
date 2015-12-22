# Create ui.R and server.R
# Set working directory to app directory
# Test: library(shiny); runApp() (type into console)
# Deploy: library(rsconnect); deployApp()

library(shiny)
library(leaflet)
library(RColorBrewer)


shinyUI(fluidPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  titlePanel("Taxis in Singapore from 7th to 15th July 2015"),
  absolutePanel(top = 10, right = 10,
                sliderInput(inputId = "range1",
                            label = "Choose the parameter that smoothens the data.",
                            min = 10,
                            max = 100,
                            value = 50, step = 10),
                sliderInput("range2", "Choose the parameter that estimates peak and valley taxi availabilities.", 0, 1,
                            value = 0.1, step = 0.1),
                selectInput("areacode", "Choose the area code whose taxi activities you want to see (1 - 55). (Data is unavailable for Area 26; choosing that will cause the app to crash.)", choices = c(1:55)),
                tableOutput("areas")
              ),
  mainPanel(h4("Map of your chosen neighborhood in Singapore"),
            h6("(The size of the grey circle represents the largest number of taxis that was ever present in this neighborhood in this week.)"),
            leafletOutput("map", width = "600px", height = "400px"),
            h1(""),
            h1(""),
            h4("Fluctuations of taxi availability in your chosen neighborhood, from 7th - 15th July 2015"),
            plotOutput("taxiplot"),
            h1(""),
            h1(""),
            h4("Documentation"),
            h6("Taxis are a main form of public transport in Singapore. All taxis in the country have GPS installed so their locations are tracked."),
            h6(""),
            h6(""),
            h6("This app helps a user visualize free-for-hire taxi distribution across Singapore from 7th - 15th July 2015. A user is able to input the following:"),
            h6("1. A parameter for smoothening the taxi data. The larger the parameter, the more data points get averaged over, and the smoother is the result."),
            h6("2. A parameter for finding peaks and valleys in the cyclical data. The higher the parameter (threshold for finding peak and valley positions), the less local positions are found."),
            h6("3. The area code for the neighborhood the user is interested in."),
            h6(""),
            h6(""),
            h6("The app presents the following output:"),
            h6("1. A map visualization of the selected neighborhood, with"),
            h6("- a popup labelling the area's name"),
            h6("- a grey circle whose size represents the highest number of taxis that was recorded for this neighborhood over the week"),
            h6(""),
            h6("2. A plot illustrating the fluctuations in free-for-hire taxis in the selected neighborhood."),
            h6(""),
            h6(""),
            h6("For details of data getting, cleaning and further analysis, please refer to the full [documentation](http://www.rpubs.com/Azzurra/taxi2) published on rpubs."))
))
