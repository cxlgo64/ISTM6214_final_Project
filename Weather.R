
rideBoston <- read.csv("Uber and Lyft Dataset Boston.csv")

#3 Weather data
weatherdata <- subset(rideBoston,select = c('cab_type','name','price','temperature','humidity','windSpeed'))


lmTemp = lm(price~temperature+humidity+windSpeed, data=weatherdata)
summary(lmTemp)


weatherdata <- subset(weatherdata,name!="WAV" & name!="Taxi")

weatherdata$group <- "Group 4 - Lux"
weatherdata$group[weatherdata$name=="Lyft" | weatherdata$name=="UberX"] <- "Group 1"
weatherdata$group[weatherdata$name=="Lyft XL" | weatherdata$name=="UberXL"] <- "Group 2 - XL"
weatherdata$group[weatherdata$name=="Shared" | weatherdata$name=="UberPool"] <- "Group 3 - Share"

#Temperature
weatherdata$temp[weatherdata$temperature<=32] <- "Low"
weatherdata$temp[weatherdata$temperature>32] <- "Normal"

ride_T <- weatherdata %>% group_by(cab_type,temperature,group,temp) %>% summarise(ride_n=n(),avgP=mean(price))

ggplot(ride_T,aes(temperature,ride_n))+geom_jitter(aes(color=cab_type))+facet_grid(temp~group)
ggplot(ride_T,aes(temperature,avgP))+geom_jitter(aes(color=cab_type))+facet_grid(temp~group)  

#Humidity
ride_H <- weatherdata %>% group_by(cab_type,humidity,group) %>% summarise(ride_n=n(),avgP=mean(price))

ggplot(ride_H,aes(humidity,ride_n))+geom_line(aes(color=cab_type))+facet_grid(.~group)
ggplot(ride_H,aes(humidity,avgP))+geom_line(aes(color=cab_type))+facet_grid(.~group)

#Wind
ride_W <- weatherdata %>% group_by(cab_type,windSpeed,group) %>% summarise(ride_n=n(),avgP=mean(price))

ggplot(ride_W,aes(windSpeed,ride_n))+geom_point(aes(color=cab_type))+facet_grid(.~group)
ggplot(ride_W,aes(windSpeed,avgP))+geom_point(aes(color=cab_type))+facet_grid(.~group)

