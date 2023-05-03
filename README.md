# Uber Analysis 🚘



![image](https://user-images.githubusercontent.com/108307724/234381846-1cdded4d-6b19-4185-bbd7-59489a5d0998.png)
# Introduction 
For this project I analyzed the number of Uber rides and displayed trends in the day, week, month and year.

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
![Image 4-25-23 at 3 27 PM](https://user-images.githubusercontent.com/108307724/234396252-4493a9da-528e-41a9-9de2-358fe15fe2d8.jpeg)




# Data Analysis 📊

To create the charts, I grouped the table by each respective time format needed(hour, month, day of week) and used each one to create a chart.
```R
#Chart 1:By the Hour 

data_by_hour <- data_combined %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())

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

ggplot(data_by_month, aes(month, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by  Month") + 
  scale_y_continuous(labels = comma)

#Chart3:Trips by hour and Month
month_hour_data <- data_combined %>% 
  group_by(month, hour) %>%  
  dplyr::summarize(Total = n())

ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)


#Chart4:Trips everyday of the Month(Max 31 days)
data_by_day <- data_combined %>% group_by(day) %>% dplyr::summarize(Trips = n())
ggplot(data_by_day, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "purple") +
  ggtitle("Trips by day of the month") + 
  scale_y_continuous(labels = comma)


#Chart5:Day of week and Month

day_month_data <- data_combined %>% 
  group_by(dayofweek, month) %>% 
  dplyr::summarize(Trips = n())


ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day of Week and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

# Chart 6 :Trips by Bases and Month 
Base_month_data <- data_combined %>% group_by(month,Base) %>% dplyr::summarize(Total = n())

ggplot(Base_month_data, aes(Base, Total, fill=month)) + 
  geom_bar(stat = "Identity",aes(fill = month), position = "dodge" ) + 
  ggtitle("Trips by Base and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)


```
Similarly, I  grouped the table by each respective time format needed(hour, month, day of week) to create heat maps as follows:

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
```R

# Shiny for charts & Heatmaps

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
      geom_bar(stat = "Identity",aes(fill = month), position = "dodge" ) + 
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
    "This chart show sus the trend of number of rides within a week along with the months which they were in. The highest number of rides were on Tuesdays, Fridays and Saturdays while the lowest were on Sundays. This is probably due to people staying home on Sundays as it is the weekend and People going out on Fridays and Saturdays. I can't hypothesize why there's such a high number on Tuesdays but this chart brings that question forth. "
  })
  output$explanation6 <- renderText({
    "This chart shows us the trend by base and month. We see that base B02617 has teh most trips and base B02512 has the least."
  })
  output$Heat1exp <- renderText({
    "In this heat map we can see that its lighter for hours 7 to8 and even lighter for hours 16 to 18.Since the label shows lighter is more trips we can coclude taht those times have more trips in a day. Moreover, day 31 is the darkest so that has the least trips in a month."
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
    "This heat map shows that the end of the week and year have a higher number of rides"
  })
  
  
  
  
  
  
  
  
}    

# Run the app
shinyApp(ui, server) 
```
<img width="1280" alt="Screenshot 2023-04-27 at 6 44 06 PM" src="https://user-images.githubusercontent.com/108307724/235013385-71b5531f-5fd4-4f0c-b81d-c3b00e299933.png">
<img width="1280" alt="Screenshot 2023-04-27 at 6 44 23 PM" src="https://user-images.githubusercontent.com/108307724/235013408-1b582486-b406-481a-ace1-c7ea5cb2bfdc.png">



# Conclusion
1. There is a significant increase in teh number of uber rides from earlier in the day to 4pm/ 5pm which could be expalined by the end of the the work day. </br>
2. The number of rides has an increasing trend as we go later in the year. Some reasons for this could be the weather or which period of the school year it is. </br>
3. There is also a pattern looking at the rides in a week. Fridays and Saturdays have a higher number possibly due to people going out, meeting up or getting groceries after the work week is over. As for Tuesday, one hypotheis could be that people with hybrid schedules are more likeley to take Monday or Fridays off as oppose dto the middle of the week as it adds to their weekend. </br>







