# Uber Analysis

For this project I analyzed the number of Uber rides and displayed trends in the day, week, month and year.
![image](https://user-images.githubusercontent.com/108307724/234381846-1cdded4d-6b19-4185-bbd7-59489a5d0998.png)


# Combining and Cleaning  the Data
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

#Data Summary
![Image 4-25-23 at 3 27 PM](https://user-images.githubusercontent.com/108307724/234396252-4493a9da-528e-41a9-9de2-358fe15fe2d8.jpeg)




#Data Analysis

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


#Conclusion






