# Uber Ride Analysis 🚘


![image](https://user-images.githubusercontent.com/108307724/236007922-846f937f-2497-4704-891f-66d503557c5d.png)


# Introduction 📄

Welcome to my GitHub repository showcasing an analysis of Uber ride data from September to April. This project aimed to explore patterns and trends in Uber rides using various data analysis tools and techniques.I utilized pivot tables, charts, heatmaps, and a web application to display my findings.

Through this project, my goal was  to gain insights on the demand for Uber rides in different timeframes, as well as to identify any factors that may affect ride demand. To accomplish this, I first cleaned and organized the data, ensuring its accuracy and completeness. I then used pivot tables to summarize the data and identify trends in ride frequency.

To visually represent my findings, I created charts and heatmaps that conveyed the distribution of Uber rides across different areas and times. Finally, I developed a web application that allowed users to interact with the data and explore the insights I uncovered.

# Data Dictionary 📕

1.</br>
Variable Name: Date.Time </br>
Description: The date and time when an Uber ride started.</br>
Data Type: Datetime</br>
2. </br>
Variable Name: Lat</br>
Description: The latitude of the location where an Uber ride started.</br>
Data Type: Numeric</br>
3.</br>
Variable Name: Lon</br>
Description: The longitude of the location where an Uber ride started.</br>
Data Type: Numeric</br>
4.</br>
Variable Name: Base</br>
Description: The Uber base code corresponding to the company that provided the ride.</br>
Data Type: Categorical (text or factor)</br>


# Data Cleaning 🧹
```R
AprilData<- read.csv("uber-raw-data-apr14.csv")
AugustData<-read.csv("uber-raw-data-aug14.csv")
JulyData<- read.csv("uber-raw-data-jul14.csv")
JunData<- read.csv("uber-raw-data-jun14.csv")
MayData<- read.csv("uber-raw-data-may14.csv")
SepData<- read.csv("uber-raw-data-sep14.csv")

colors = c("#CC1012", "#05a399", "#cfcaca", "#665535","#f5e840", "#0683c9", "#e075b0")

data_combined <- rbind(AprilData,AugustData,MayData,JunData,JulyData,SepData)

data_combined$Date.Time <- as.POSIXct(data_combined$Date.Time, format="%m/%d/%Y %H:%M:%S")
data_combined$Time <- format(as.POSIXct(data_combined$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_combined$Date.Time <- ymd_hms(data_combined$Date.Time)

data_combined$day <- factor(day(data_combined$Date.Time))
data_combined$month <- factor(month(data_combined$Date.Time, label=TRUE))
data_combined$year <- factor(year(data_combined$Date.Time))
data_combined$dayofweek <- factor(wday(data_combined$Date.Time, label=TRUE))

data_combined$second = factor(second(hms(data_combined$Time)))
data_combined$minute = factor(minute(hms(data_combined$Time)))
data_combined$hour = factor(hour(hms(data_combined$Time)))

```

# Data Summary 📁
Here is a representative example of one of the original data sets, corresponding to the month of April. I started this analysis with 6 total similar datasets for six months out of the year.
| Date.Time          | Lat      | Lon      | Base   |
|--------------------|----------|----------|--------|
| 4/1/2014 0:11:00   | 40.7690  | -73.9549 | B02512 |
| 4/1/2014 0:17:00   | 40.7267  | -74.0345 | B02512 |
| 4/1/2014 0:21:00   | 40.7316  | -73.9873 | B02512 |
| 4/1/2014 0:28:00   | 40.7588  | -73.9776 | B02512 |
| 4/1/2014 0:33:00   | 40.7594  | -73.9722 | B02512 |

# Data Analysis 📊

In order to analyze trends from the dataset, I created  pivot tables, charts and heat maps for the required time frames, namely by hour, by month, and by week. Each pivot table was then written to a CSV file for further analysis and visualization.

```R
#Chart 1:By the Hour 

data_by_hour <- data_combined %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())

write.csv(data_by_hour, "data_by_hour.csv", row.names = FALSE)

ggplot(data_by_hour, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="pink", 
           color="blue") + 
  ggtitle("Trips by the Hour") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)

#Chart2: Trips by Month

data_by_month <- data_combined %>% 
  group_by(month) %>% 
  dplyr::summarize(Total = n())
write.csv(data_by_month, "data_by_month.csv", row.names = FALSE)
ggplot(data_by_month, aes(month, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by  Month") + 
  scale_y_continuous(labels = comma)

#Chart3:Trips by hour and Month
month_hour_data <- data_combined %>% 
  group_by(month, hour) %>%  
  dplyr::summarize(Total = n())
write.csv(month_hour_data, "month_hour_data.csv", row.names = FALSE)
ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)


#Chart4:Trips everyday of the Month(Max 31 days)
data_by_day <- data_combined %>% group_by(day) %>% dplyr::summarize(Trips = n())
write.csv(data_by_day, "data_by_day.csv", row.names = FALSE)
ggplot(data_by_day, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "purple") +
  ggtitle("Trips by day of the month") + 
  scale_y_continuous(labels = comma)


#Chart5:Day of week and Month

day_month_data <- data_combined %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
write.csv(day_month_data, "day_month_data.csv", row.names = FALSE)

ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day of Week and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

# Chart 6 :Trips by Bases and Month 
Base_month_data <- data_combined %>% group_by(month,Base) %>% dplyr::summarize(Total = n())
write.csv(Base_month_data, "Base_month_data.csv", row.names = FALSE)
ggplot(Base_month_data, aes(Base, Total, fill=month)) + 
  geom_bar(stat = "Identity",aes(fill = month), position = "dodge" ) + 
  ggtitle("Trips by Base and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

```
Similarly, I  created more pivot tables by each respective time frame needed(hour, month, day of week) to create heat maps as follows:

```R
# Heatmap1:by hr and day
day_hour_data <- data_combined %>% group_by(day, hour) %>% dplyr::summarize(Total = n())
ggplot(day_hour_data, aes(day, hour, fill = Total)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map of Trips by Hour and Day")

# Heatmap2:by month and day,

month_day_data <- data_combined %>% group_by(month, day) %>% dplyr::summarize(Trips = n())
ggplot(month_day_data, aes(day, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map of Trips by Month and Day")

#Heatmap3:by month and week day,

ggplot(day_month_data, aes(dayofweek, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map of Trips by Month and Week Day")

#Heatmap4:by Bases and weekday
Base_week_data <- data_combined %>% group_by(dayofweek,Base) %>% dplyr::summarize(Total = n())
ggplot(Base_week_data , aes(dayofweek, Base, fill = Total)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Base and Day of Week") 
  
  

#Heatmap5:By month and Week day
ggplot(day_month_data, aes(month, dayofweek, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Week Day")

```
# Creating the Shiny App 🌐

I created a separate web app Rscript to create and deplot the shiny app
```R
colors = c("#CC1012", "#05a399", "#cfcaca", "#665535","#f5e840", "#0683c9", "#e075b0")

#Read the csv files for the charts

data_by_hour<- read.csv("data_by_hour.csv")
data_by_month<-read.csv("data_by_month.csv")
month_hour_data<-read.csv("month_hour_data.csv")
data_by_day<-read.csv("data_by_day.csv")
Base_month_data<-read.csv("Base_month_data.csv")
day_hour_data<-read.csv("day_hour_data.csv")
month_day_data<-read.csv("month_day_data.csv")
Base_week_data<-read.csv("Base_week_data.csv")
data_by_day<-read.csv("data_by_day.csv")
day_month_data<-read.csv("day_month_data.csv")

ui <- fluidPage(
  
  # Add title to the app
  titlePanel("Uber Rides Analysis"),
  
  #Display chart 1
  plotOutput("chart1"),
  
  textOutput("explanation1"),
  
  #Display chart 2
  plotOutput("chart2"),
  
  textOutput("explanation2"),
  
  # Display chart 3
  plotOutput("chart3"),
  
  textOutput("explanation3"),
  
  # Display chart 4
  plotOutput("chart4"),
  
  textOutput("explanation4"),
  
  # Display chart 5
  plotOutput("chart5"),
  
  textOutput("explanation5"),
  
  # Display chart 6
  plotOutput("chart6"),
  
  textOutput("explanation6"),
  
  # Display Heat map 1
  plotOutput("Heatmap1"),
  
  textOutput("Heat1exp"),
  
  # Display Heat map 2
  plotOutput("Heatmap2"),
  
  textOutput("Heat2exp"),
  # Display Heat map 3
  plotOutput("Heatmap3"),
  
  textOutput("Heat3exp"),
  # Display Heat map 4
  plotOutput("Heatmap4"),
  
  textOutput("Heat4exp"),
  # Display Heat map 5
  plotOutput("Heatmap5"),
  
  textOutput("Heat5exp"),
  

  titlePanel("Geospatial Leaflet: Rides in New York"),
# Leaflet map and info box
mainPanel(
  leafletOutput("map"),
  verbatimTextOutput("info")
)
)


# Define the server
server <- function(input, output) {
  
  #Render the chart 1
  
  output$chart1 <- renderPlot({
    ggplot(data_by_hour, aes(hour, Total)) + 
      geom_bar(stat="identity", 
               fill="pink", 
               color="blue") + 
      ggtitle("Trips by the Hour", subtitle = "aggregated today") + 
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5)) + 
      scale_y_continuous(labels=comma)
    
  })
  
  #Render chart 2
  output$chart2 <- renderPlot({
    ggplot(data_by_month, aes(month, Total, fill=month)) + 
      geom_bar(stat = "identity") + 
      ggtitle("Trips by  Month") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      scale_y_continuous(labels = comma)
  })
  
  # Render chart 3
  output$chart3 <- renderPlot({
    
    ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
      geom_bar(stat = "identity") + 
      ggtitle("Trips by Hour and Month") + 
      theme(plot.title = element_text(hjust = 0.5),legend.position = "none") + 
      scale_y_continuous(labels = comma)
  })
  
  # Render chart 4
  output$chart4 <- renderPlot({
    ggplot(data_by_day, aes(day, Trips)) + 
      geom_bar(stat = "identity", fill = "purple") +
      ggtitle("Trips by day of the month") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      scale_y_continuous(labels = comma)
  })
  
  # Render chart 5
  output$chart5 <- renderPlot({
    ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
      geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
      ggtitle("Trips by Day of Week and Month") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      scale_y_continuous(labels = comma) + 
      scale_fill_manual(values = colors)
  })
  
  # Render chart 6
  output$chart6 <- renderPlot({
    ggplot(Base_month_data, aes(Base, Total, fill=month)) + 
      geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
      ggtitle("Trips by Base and Month") +
      theme(plot.title = element_text(hjust = 0.5)) + 
      scale_y_continuous(labels = comma) + 
      scale_fill_manual(values = colors)
    
  })
  
  # Render Heatmap 1
  output$Heatmap1 <- renderPlot({
    ggplot(day_hour_data, aes(day, hour, fill = Total)) + 
      geom_tile(color = "white") + 
      ggtitle("Heat Map of Trips by Hour and Day")
    
    
  })
  # Render Heatmap 2
  output$Heatmap2 <- renderPlot({
    ggplot(month_day_data, aes(day, month, fill = Trips)) + 
      geom_tile(color = "white") + 
      ggtitle("Heat Map of Trips by Month and Day")
    
  })
  # Render Heatmap 3
  output$Heatmap3 <- renderPlot({
    ggplot(day_month_data, aes(dayofweek, month, fill = Trips)) + 
      geom_tile(color = "white") + 
      ggtitle("Heat Map of Trips by Month and Week Day")
    
    
  })
  # Render Heatmap 4
  output$Heatmap4 <- renderPlot({
    ggplot(Base_week_data , aes(dayofweek, Base, fill = Total)) + 
      geom_tile(color = "white") + 
      ggtitle("Heat Map by Base and Day of Week") 
    
    
  })
  # Render Heatmap 5
  output$Heatmap5 <- renderPlot({
    ggplot(day_month_data, aes(month, dayofweek, fill = Trips)) + 
      geom_tile(color = "white") + 
      ggtitle("Heat Map by Month and Week Day")
    
  })
  
  output$explanation1 <- renderText({
    "This chart shows the number of rides for each hour in a day. The trend in the chart show sus there is a significant increase in rides from hour 16 to 17. This is 4pm to 5pm, which makes sense because that's when the work day ends and people need rides home."
  })
  
  output$explanation2 <- renderText({
    "This chart indicates an increasing trend going from April to September."
  })
  
  output$explanation3 <- renderText({
    "This chart sums up what we saw in the first two charts. There's more rides in the afternoons and in september while there's less rides in the mornings and in April. Looking at these together can help us hypothesize the reasons for these occurances such as The beginning of the school year in Spetemeber correlating with the number of rides or that people take more uber rides in the afternoons(when the day is over) than when the morning or evening. "
    
  })
  
  output$explanation4 <- renderText({
    "This chart shows less of a trend than the earlier charts. One thing that stands out is the decrease of day 31, however this is probably due to fact that only half the months in a year have 31 days."
  })
  
  output$explanation5 <- renderText({
    "This chart shows us the trend of number of rides within a week along with the months which they were in. The highest number of rides were on Tuesdays, Fridays and Saturdays while the lowest were on Sundays. This is probably due to people staying home on Sundays as it is the weekend and People going out on Fridays and Saturdays. I can't hypothesize why there's such a high number on Tuesdays but this chart brings that question forth. "
  })
  output$explanation6 <- renderText({
    "This chart shows us the trend by base and month. We see that base B02617 has teh most trips and base B02512 has the least."
  })
  output$Heat1exp <- renderText({
    "In this heatmap we can see that its lighter for hours 7 to8 and even lighter for hours 16 to 18.Since the label shows lighter is more trips we can coclude taht those times have more trips in a day. Moreover, day 31 is the darkest so that has the least trips in a month."
  })
  output$Heat2exp <- renderText({
    "Darker colors in July,May and April show a decrease compare dto other months. This calendar view reveals how numbre of trips is affected during holidays like July 4,and Easter "
  })
  output$Heat3exp <- renderText({
    "Here we see lighter colors on the top right showing us that there's more rides taken at the end of the work week and also at the end the year."
  })
  output$Heat4exp <- renderText({
    "There is a clear distinction B02598, B02617 and B02682 have the most rides while B02512 and B02764 have the least."
  })
  output$Heat5exp <- renderText({
    "This heatmap shows that the end of the week and year have a higher number of rides"
  })
  
  
  
  # Initialize leaflet map with default view
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -74.0060, lat = 40.7128, zoom = 13)
  })
  
  # Add markers with pop-up info based on data frame
  data <- data.frame(
    name = c("Statue of Liberty", "Empire State Building"),
    lat = c(40.6892, 40.7484),
    lng = c(-74.0445, -73.9857),
    info = c("The Statue of Liberty is a colossal neoclassical sculpture on Liberty Island in New York Harbor within New York City.", "The Empire State Building is a 102-story Art Deco skyscraper in Midtown Manhattan, New York City.")
  )
  
  # Create reactive values for markers and search results
  markers <- reactiveValues(data = data)
  searchResults <- reactiveValues(data = NULL)
  
  # Add markers to map
  observe({
    leafletProxy("map", data = markers$data) %>%
      clearMarkers() %>%
      addMarkers(lng = ~lng, lat = ~lat, popup = ~name)
  })
  
  # Update markers based on search results
  observe({
    if (!is.null(searchResults$data)) {
      leafletProxy("map", data = searchResults$data) %>%
        clearMarkers() %>%
        addMarkers(lng = ~lng, lat = ~lat, popup = ~name)
    }
  })
  
  # Search button action
  observeEvent(input$go, {
    if (input$search != "") {
      searchResults$data <- markers$data[grep(input$search, markers$data$name), ]
      if (nrow(searchResults$data) == 0) {
        showNotification("No results found.", type = "warning", duration = 3)
      } else {
        leafletProxy("map") %>%
          fitBounds(lng1 = min(searchResults$data$lng), lat1 = min(searchResults$data$lat),
                    lng2 = max(searchResults$data$lng), lat2 = max(searchResults$data$lat))
      }
    }
  })
  
  # Reset map button action
  observeEvent(input$reset, {
    js$resetMap()
    searchResults$data <- NULL
  })
  
  # Measure button action
  observeEvent(input$measure, {
    leafletProxy("map") %>%
      measure(type = "polyline", primaryLengthUnit = "meters")
  })
  
    
}
# Run the app
shinyApp(ui, server) 

 



# Conclusion 🏁

1. A statistically significant increase is observed in the number of Uber rides from earlier in the day until 4pm/5pm, which could potentially be attributed to the end of the workday.</br>

2. An increasing trend is observed in the number of Uber rides as we move later in the year. Possible factors contributing to this trend could be weather patterns or the academic calendar.</br>

3. A discernible pattern is also observed in the distribution of rides over the week. Fridays and Saturdays demonstrate a higher volume of rides, which may be indicative of people socializing or running errands after the work week. Alternatively, one hypothesis is that people with hybrid schedules tend to take Mondays or Fridays off, thus extending their weekends. This may explain the lower number of rides observed on Tuesdays, which falls in the middle of the week.</br>







