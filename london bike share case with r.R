
---
  title: 'London Bike Share Analysis with R'
author: 'Ece Gazioğlu'
date: '`r Sys.Date()`'
output:
  html_document:
  number_sections: true
toc: true
---
  
  ## 1- DATASET
  
  This dataset stored in Kaggle names London Bike-Share Usage Dataset and was made available through Svetlana Kalacheva.The dataset contains detailed records of 776,527 bicycle journeys from the Transport for London (TfL) Cycle Hire system spanning from August 1 to August 31, 2023. The TfL Cycle Hire initiative provides publicly accessible bicycles for rent across London, promoting sustainable transportation and physical fitness. This dataset provides a snapshot of cycling activity during the month, including start and end details for each journey, the bicycle used, and the duration of hire.


* Reliability: This dataset was sourced directly from the Transport for London's official website, which provides open data to encourage public use and analysis.
* Comprehensive: No, a small dataset with small amount of variables.
* Current: No, spanning from August 1 to August 31, 2023.
* Cited: Yes, Transport for London. (August 2023). TfL Cycle Hire Trip Data. Retrieved [Date Retrieved], from https://tfl.gov.uk/info-for/open-data-users/our-open-data.

## 2- PROCESS 

### 2.1- INSTALLING PACKAGES

```{r,warning=F,message=F,results='hide'}
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("cleaner")
install.packages("readr")
install.packages("knitr")
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cleaner)
library(readr)
library(knitr)
```

### 2.2- IMPORTING DATASETS

```{r,warning=F,message=F,results='hide'}
bike <- read_csv("/kaggle/input/london-bike-share-usage-dataset/LondonBikeJourneyAug2023.csv")
```

### 2.3- PREVIEWING DATASETS

```{r,message=F,warning=F}
head(bike)
str(bike)
```

776527 obs, 11  variables
There are some format issues, start date and end date are in character format, we need to change them into POSIXct.

### 2.4- CLEANING AND FORMATING DATASET

#### 2.4.1- CLEANING AND RENAMING COLUMNS

```{r,message=F,warning=F}
colnames(bike) <- format_names(colnames(bike),snake_case = T)
colnames(bike)
```

#### 2.4.2- DETERMINING SOME USEFUL NUMBERS

```{r,message=FALSE,warning=F}
bike %>% 
  group_by(number) %>% 
  summarize(count=n()) 
### 776527 bikes
table(bike$bike_model)
### there are two types of bikes
bike %>% 
  group_by(start_station_number) %>% 
  summarize(count=n())
### 800 start station
bike %>% 
  group_by(end_station_number) %>% 
  summarize(count=n()) 
### 802 end station
stations <- union(bike$start_station,bike$end_station)
summary(stations) 
### there are 802 stations
sum(unique(bike$start_station) == stations)
setdiff(stations,unique(bike$start_station))
bike$start_station[str_detect(bike$start_station,"Mechanical Workshop")]
### these two stations can take returned bikes but not rent/loan bikes
```

#### 2.4.3- DETECTING AND CONTROLLING NA'S

```{r,message=F,warning=F}
summary(bike)   
### no NA's
```

#### 2.4.4- DETECTING DUPLICATES

```{r,message=F,warning=F}
bike %>% 
  group_by(number,start_date,end_date,start_station_number,end_station_number) %>% 
  summarize(count=n()) %>% 
  filter(count>1)   
### no duplicates
```

#### 2.4.5- MAKING COLUMNS IN A CONSISTENT FORMAT

```{r}
bike$start_date <- mdy_hm(bike$start_date)
bike$end_date <- mdy_hm(bike$end_date)
```

## 3- ANALYSIS

```{r}
### The most busy day
bike %>% 
  group_by(day(start_date)) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))
```

August 9th, Wednesday, a mild, 23 celcius day in London. The least busy day, August 5th, Saturday, there was drizzled weather in London that day.

```{r}
bike %>% 
  group_by(day=wday(start_date,label=T,locale="ENG")) %>% 
  summarize(count=n()) %>% 
  arrange(count) %>% 
  ggplot(aes(x=day,y=count))+
  geom_col()+
  labs(title="Usage Amount of Bikes",
       x="Days",
       y="Usage Amount of Bikes")
```

The most used days are weekdays, the least using days are weekends. Why?
  
  ```{r} 
bike %>% 
  group_by(hour=hour(start_date)) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

bike %>% 
  group_by(hour=hour(start_date)) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% 
  ggplot(aes(x=hour,y=count))+
  geom_col()+
  labs(title="Usage Amount of Bikes",
       x="Hours",
       y="Usage Amount of Bikes")
```

The most used hours are can be considered going to work and going out of work hours,so it can be said that, in 2023 August, in London, the people mostly prefer bikes as vehicles for going work.

```{r,message=F,warning=F}
bike %>% 
  group_by(wday(start_date,label=T,locale="ENG")) %>% 
  summarize(avgdrt=mean(total_duration_ms_)/60000) %>% 
  arrange(desc(avgdrt)) 
```

Weekends have the longest durations.

```{r,message=F,warning=F}  
bike %>% 
  group_by(day=wday(start_date,label=T,locale="ENG")) %>% 
  summarize(avgdrt=mean(total_duration_ms_)/60000) %>% 
  arrange(desc(avgdrt)) %>% 
  ggplot(aes(x=day,y=avgdrt))+
  geom_col()+
  labs(title="Average Duration of Trips in Days",
       x="Days",
       y="Average Duration in Minutes")
```

We can say, according to usage amount and durations, people choose bicycles to go somewhere spesific i.e. to work in weekdays, but according to durations people prefer bicycles to take a joyful tour.

The most used routes;

```{r,message=F,warning=F}
bike %>% 
  group_by(start_station,end_station) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

head(bike %>% 
       group_by(start_station) %>% 
       summarize(count=n()) %>% 
       arrange(desc(count)),20) %>% 
  ggplot(aes(x=count,y=reorder(start_station,count)))+
  geom_col()+
  scale_x_continuous(breaks=c(2000,3000,4000,5000,6000,7000))+
  labs(title="Top 20 Starting Stations",
       x="Count",
       y="Stations")

head(bike %>% 
       group_by(end_station) %>% 
       summarize(count=n()) %>% 
       arrange(desc(count)),20) %>% 
  ggplot(aes(x=count,y=reorder(end_station,count)))+
  geom_col()+
  scale_x_continuous(breaks=c(2000,3000,4000,5000,6000,7000))+
  labs(title="Top 20 Ending Stations",
       x="Count",
       y="Stations")
```

We can say that, the people mostly prefer touring Hyde Park. So, it could be wise to invest there (such as making stations around there bigger) 


These are the most used bikes, maybe they need some maintenance and renovation;

```{r,message=F,warning=F}
bike %>% 
  group_by(bike_number) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))
```

```{r,message=F,warning=F}
bike %>% 
  group_by(bike_model) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=bike_model,y=count))+
  geom_col()+
  scale_y_continuous(n.breaks=15)
```

The people mostly prefer classic bikes, but this might be because there are relatively very few e-bikes.

```{r,message=F,warning=F}
bike %>% 
  group_by(day=day(start_date),bike_model) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=day,y=count,color=bike_model))+
  geom_line(linewidth=1)+
  geom_smooth(method="lm",se=F)+
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30))+
  scale_y_continuous(breaks=c(2500,5000,7500,10000,17500,20000,22500,25000,27500,30000))
```

Here we can see that the people mostly prefer classic bikes, too. And also we can say that total bike user amount didn't change and there are no preference shift between classic and e-bikes since ups and downs of both classical and e-bike usage time serie in the graph are symmetrical. But of course we should keep in mind the data covers only one month period.

  
