library(tidyverse)
library(lubridate)
library(ggplot2)

# load datasets
df_202201 <- read_csv('202201-divvy-tripdata.csv')
df_202202 <- read_csv('202202-divvy-tripdata.csv')
df_202203 <- read_csv('202203-divvy-tripdata.csv')
df_202204 <- read_csv('202204-divvy-tripdata.csv')
df_202205 <- read_csv('202205-divvy-tripdata.csv')
df_202206 <- read_csv('202206-divvy-tripdata.csv')
df_202207 <- read_csv('202207-divvy-tripdata.csv')
df_202208 <- read_csv('202208-divvy-tripdata.csv')
df_202209 <- read_csv('202209-divvy-tripdata.csv')
df_202210 <- read_csv('202210-divvy-tripdata.csv')
df_202211 <- read_csv('202211-divvy-tripdata.csv')
df_202212 <- read_csv('202212-divvy-tripdata.csv')

# combine all dataframes into one dataframe
df_2022 <- bind_rows(df_202201,df_202202,df_202203,df_202204,df_202205,df_202206,
                     df_202207,df_202208,df_202209,df_202210,df_202211,df_202212)
View(df_2022)

# check for missing data
colSums(is.na(df_2022))

# remove columns that are not needed
df_2022 <- df_2022 %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng,
            start_station_id,end_station_id,start_station_name,end_station_name))

# review data
colnames(df_2022)
dim(df_2022)
str(df_2022)
summary(df_2022)
head(df_2022)

# check for number of member/casual and rideable type
table(df_2022$member_casual)
table(df_2022$rideable_type)

# add separate columns for date and time
df_2022$date <- as.Date(df_2022$started_at)
df_2022$month <- format(as.Date(df_2022$date),'%m')
df_2022$day <- format(as.Date(df_2022$date),'%d')
df_2022$year <- format(as.Date(df_2022$date),'%Y')
df_2022$day_of_week <- format(as.Date(df_2022$date),'%A')
df_2022$time <- format(as.POSIXct(df_2022$started_at),format='%H:%M')

# calculated field to show the time length of each unique ride
df_2022$ride_length <- (difftime(df_2022$ended_at,df_2022$started_at,units='mins'))
df_2022$ride_length <- as.numeric(as.character(df_2022$ride_length))
# check for negative length rides and delete them
table(df_2022$ride_length<0)
df_2022 <- df_2022[!(df_2022$ride_length<0),]
# check summary of new column
summary(df_2022$ride_length)

# save cleaned dataset
write.csv(df_2022,file='/Users/daniel/Document/Coding/bike_share/bike_share_2022_cleaned.csv')

# calculate the mean, median, max, min for each type of customer
aggregate(df_2022$ride_length ~ df_2022$member_casual, FUN = mean)
aggregate(df_2022$ride_length ~ df_2022$member_casual, FUN = median)
aggregate(df_2022$ride_length ~ df_2022$member_casual, FUN = max)
aggregate(df_2022$ride_length ~ df_2022$member_casual, FUN = min)

# average ride time by day of week
aggregate(df_2022$ride_length ~ df_2022$member_casual + df_2022$day_of_week, FUN = mean)
# order the days of week
df_2022$day_of_week <- ordered(df_2022$day_of_week,
                               levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

# analyze by type of customers and days of the week
df_2022 %>% 
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday)

# analyze by type of customers and month
print(df_2022 %>% 
        group_by(member_casual,month) %>% 
        summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% 
        arrange(member_casual,month),n=24)

# analyze by type of bike
df_2022 %>% 
  group_by(member_casual,rideable_type) %>% 
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,rideable_type)

# bar chart of number of rides by day of week
df_2022 %>% 
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n()) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual)) + geom_col(position='dodge') + 
  labs(x='Day of Week',y='Total Number of Rides',title='Rides per Day of Week',fill='Membership Type') +
  theme(plot.title=element_text(hjust=0.5)) + scale_fill_brewer(palette='Set2') +
  scale_y_continuous(breaks=c(100000,200000,300000,400000,500000),labels=c('100k','200K','300K','400K','500K'))

# bar chart of average duration of members/casuals
df_2022 %>% 
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=average_duration,fill=member_casual)) + geom_col(position='dodge') +
  labs(x='Day of Week',y='Average Duration (Min)',title='Average Ride Duration per Day of Week',fill='Membership Type') +
  theme(plot.title=element_text(hjust=0.5)) + scale_fill_brewer(palette='Set2')

# bar chart of number of rides per month
df_2022 %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides=n()) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=month,y=number_of_rides,fill=member_casual)) + geom_col(position='dodge') +
  labs(x='Month',y='Total Number of Rides',title='Total Number of Rides per Month',fill='Membership Type') +
  theme(plot.title=element_text(hjust=0.5)) + scale_fill_brewer(palette='Set2') +
  scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) +
  scale_y_continuous(breaks=c(100000,200000,300000,400000),labels=c('100K','200K','300K','400K'))

# bar chart of number of rentals by type of bike
df_2022 %>% 
  ggplot(aes(x=rideable_type,fill=member_casual)) + geom_bar(position='dodge') +
  labs(x='Type of Bike',y='Number of Rentals (Millions)',title='Bikes Rented',fill='Type of Membership') +
  theme(plot.title=element_text(hjust=0.5)) + scale_fill_brewer(palette='Set2') +
  scale_y_continuous(breaks=c(500000,1000000,1500000),labels=c('0.5M','1.0M','1.5M'))
