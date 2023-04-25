# March Madness

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



# Looking at the winners- Leandra

# Correlation-Hildana
```
variables <- df%>%
  select(Company, Product,Issue,State)

```

# Creating a new meteric- Leandra

# Conclusion-
Winner for each bracket
Overall winner

# Contributors
Hildana Teklegiorgis</br>
Leandra Gottschalk</br>
Mansi Gujadhur</br>




