library(shiny)
library(leaflet)
library(ggplot2)

# Data processing, load the location and hourly counts data
setwd("C:/Users/David/Desktop/29910226_Dawei_Gu")
location <- read.csv('Pedestrian_Counting_System_-_Sensor_Locations (Exercise 2).csv')
data <- read.csv('Pedestrian_Counting_System_2019 (Exercise 2).csv')

#check data
location
data

# find the average hourly counts for each sensor to create aggregate 
# dataframe
data_ag <- aggregate(Hourly_Counts ~ Sensor_Name, FUN = mean, data = data) 
data_ag

# try to match the sensor name in the aggregate dataframe with the 
# location data
new_location <- data_ag$Hourly_Counts[match(location$sensor_name,
                                            data_ag$Sensor_Name)]
new_location

# add the average data into location data
location$Hourly_Counts <- new_location
location

# remove the row without average data and check
location <- location[!is.na(location$Hourly_Counts),]
location


# find the average hourly counts for each sensor to create aggregate 
# dataframe
data_ag <- aggregate(Hourly_Counts ~ Sensor_Name+Time+Day, FUN = mean, 
                     data = data) 

# set the order for facet
target <- c("Monday", "Tuesday", "Wednesday", "Thursday", 'Friday',
            'Saturday','Sunday')
data_ag$facet <- factor(data_ag$Day, levels=target)
data_ag

# testing
test_data = data_ag[data_ag$Sensor_Name == "Spencer St-Collins St (South)", ]
test_data

  

#UI of shiny
ui <- fluidPage(
  #title of page
  titlePanel("Pedestrian Counting System Map with Statistic Line Chart"),
  
  fluidRow(
    #show the leaflet map on the left and take 7/12(58%) of screen
    column(7,leafletOutput("mymap")),
    #plot show the detail of each sensor and takes 5/12(42%) of screen
    column(5,plotOutput("linePlot"))
    ),
  #collect the data from clicking the marker in the leaflet map
  fluidRow(verbatimTextOutput("map_marker_click")
  )
)

#server of shiny
server <- function(input, output, session) {
# leaflet map
  output$mymap <- renderLeaflet({ # create leaflet map
    leaflet(data = location) %>% 
      addTiles() %>%
      fitBounds(144.9397, -37.82413, 144.9746, -37.7969)%>%
# create the circle marker and marker, the size of circle is 
# proportionate to the average hourly count of sensor
      addCircleMarkers(~longitude, ~latitude, 
                       layerId = ~sensor_name,
                       color = 'red',
                       radius = ~Hourly_Counts/150,
                       popup = ~as.character(sensor_name)) 
    
  })
  
# The ggplot of line charts of sensor
  output$linePlot <- reactivePlot(function() {
    #if user have not select the marker of sensor, the default is 
    # "Spencer St-Collins St (South)", and extract the aggregate data 
    # only for this sensor  
    if (is.null(input$mymap_marker_click$id)){
      tname <- "Spencer St-Collins St (South)"
      senor_data <- data_ag[data_ag$Sensor_Name == 
                             "Spencer St-Collins St (South)", ]
    }
    # if the user select the sensor by click marker, extract the 
    # aggregate data only for that sensor
    else{
      tname <- input$mymap_marker_click$id
      senor_data <- data_ag[data_ag$Sensor_Name == 
                             input$mymap_marker_click$id, ]
    }
    # make the plots with title
    ggplot(senor_data,
           aes(Time, Hourly_Counts)) +
      ggtitle(paste(tname,'\n (Average Hourly Pedestrian Count of day of Week)'))+
      geom_line() +
      labs(x="Hours(24-hour)",y="Average Hourly Pedestrian Count") + 
      facet_wrap(~facet)})
}

#run the shinyapp and close the messages
options(shiny.deprecation.messages=FALSE)
shinyApp(ui, server)

