#read librarys
library(ggplot2)

library(dplyr)
#load the database, please set dw before you run the read.csv code
ridedata <- read.csv("rideshare.csv")

View(ridedata)
#Data Cleansing:
#1.check the data set variable
str(ridedata)

#filter the necessary data we need in this project, which minimize the size of 
#The data set
ridedata %>%
  group_by(timezone) %>%
  summarise(n = n_distinct(cab_type)) %>%
  ggplot()  + geom_col(aes(timezone, n))
#ggplot() + geom_bar(aes(Date, n), stat = "identity")

ridedata_small <- subset(ridedata, select = -c(id,timezone,long_summary,latitude,longitude))

#2.check the missing value. As we can see the percentage of missing value is
#very low, so we are fine to just get rid of them.
sum(is.na(ridedata_small))
mean(is.na(ridedata_small))

#remove the missing values, double check
ridedata_drop <- na.omit(ridedata_small)  
sum(is.na(ridedata_drop))

View(ridedata_drop)

write.csv(ridedata_drop,"ridashare_new.csv", row.names = FALSE)

