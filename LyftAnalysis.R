#ggplot2 and dplyr
library(ggplot2)
library(dplyr)
#Read Data
rideBoston <- read.csv("ridashare_new.csv")
View(rideBoston)
summary(rideBoston)
rideBoston%>%count(cab_type,sort=TRUE)

#Get the distribution of cab in this data set
df <- data.frame(platform=c("Lyft","Uber"),freqC=c(307408,330568))

ggplot(df, aes(x="", y= freqC,fill=platform))+
geom_col(width=1)+
scale_fill_manual(values =c("red","yellow"))+
geom_bar(stat='identity')+
coord_polar("y",start=pi/3)+
labs(title="Cab platform trip distribution in Boston")


#In this part, we need to output some data based on the location, sunrise, sunset, and moonPhase

#1. location data output.
locationdata <- subset(rideBoston,select= c('cab_type','name','source','destination','price','distance'))

locationdata %>%
  group_by(name) %>%
  summarise(n = n_distinct(distance)) %>%
  ggplot()  + geom_col(aes(name, n))

qplot(x=source, y=price, data=locationdata,
      color = name,
      xlab = "Departure location",
      ylab = "Price($)")

qplot(x=destination, y=price, data=locationdata,
      color = name,
      xlab = "Destination",
      ylab = "Price($)")

#use ggplot to generate a linear regression graphs for distance and price
ggplot(locationdata,aes(x=distance, y=source, color=name)) +
  geom_point()

ggplot(locationdata,aes(x=distance, y=name, color=cab_type)) + geom_point()


ggplot(locationdata,aes(x=distance, y=source, color=name)) + 
  geom_smooth(method='lm')

ggplot(locationdata,aes(x=distance, y=name, color=cab_type)) + 
  geom_smooth(method='lm') 
#Use avPlots to generate graphs 
model2 <- lm(distance ~ source + destination , data = locationdata)
sum(is.na(model2))
summary(model2)

model3 <- lm(distance ~ price+ name , data = locationdata)
sum(is.na(model3))
summary(model3)
#2 Sunrise and sunset data
sundata <- subset(rideBoston,select= c('cab_type','name','sunriseTime','sunsetTime','moonPhase','price','distance'))
summary(sundata)

LyftSun <-subset(sundata,cab_type=="Lyft")
UberSun <-subset(sundata,cab_type=="Uber")
View(LyftSun)
qplot(x=moonPhase, y=price, data=LyftSun,
      color = name,
      shape = name,
      xlab = "moonPhase(time)",
      ylab = "ride price ($)")


qplot(x=moonPhase, y=price, data=UberSun,
      color = name,
      shape = name,
      xlab = "moonPhase(time)",
      ylab = "ride price ($)")

qplot(x=(sunsetTime-sunriseTime)/3600, y=price, data=UberSun,
      color = name,
      shape = name,
      xlab = "dayTime(time)",
      ylab = "ride price ($)")

qplot(x=(sunsetTime-sunriseTime)/3600, y=price, data=LyftSun,
      color = name,
      shape = name,
      xlab = "dayTime(time)",
      ylab = "ride price ($)")

qplot(x=sunsetTime, y=price, data=LyftSun,
      color = name,
      shape = name,
      xlab = "sunsetTime(time)",
      ylab = "ride price ($)")

qplot(x=sunsetTime, y=price, data=UberSun,
      color = name,
      shape = name,
      xlab = "sunsetTime(time)",
      ylab = "ride price ($)")


x <-count(rideBoston[rideBoston$price<15.0,])
y <-count(rideBoston)
x/y

#Use avPlots to generate graphs 
model <- lm(price ~ sunsetTime+ moonPhase , data = sundata)
summary(model)

rideBoston$name = with(rideBoston, reorder(name,distance,median))
avPlots()
library(car)
avPlots(model3)

p <- rideBoston %>%
  ggplot( aes(x=name, y=price, fill=name)) + 
  geom_violin() +
  xlab("name") +
  theme(legend.position="none") +
  xlab("")

p

rideBoston$cab_type = with(rideBoston, reorder(cab_type,price,median))
p2 <- rideBoston %>%
  ggplot( aes(x=cab_type, y=price, fill=cab_type)) + 
  geom_violin() +
  xlab("cab_type") +
  theme(legend.position="none") +
  xlab("")
p2

rideBoston %>%
  arrange(price) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  # This trick update the factor levels
  ggplot( aes(x=name, y=price)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=1, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")

#3. Time related data output
timedata <-subset(rideBoston,select= c('cab_type','name','price','timestamp','datetime','hour','day'))
model <- lm(price ~ hour+day , data = timedata)
summary(model)
Lyfttime <-subset(timedata,cab_type=="Lyft")
Ubertime <-subset(timedata,cab_type=="Uber")
qplot(x=hour, y=price, data=Lyfttime,
      color = name,
      shape = name,
      xlab = "datetime(time)",
      ylab = "ride price ($)")

qplot(x=day, y=price, data=Lyfttime,
      color = name,
      shape = name,
      xlab = "day(time)",
      ylab = "ride price ($)")
qplot(x=hour, y=count(subset(timedata,)), data=Ubertime,
      color = name,
      shape = name,
      xlab = "day(time)",
      ylab = "ride price ($)")

model6 <-Ubertime %>% group_by(hour) %>% summarise(totalrides=n(),.groups= 'drop')
model7 <-Lyfttime %>% group_by(hour) %>% summarise(totalrides=n(),.groups= 'drop')
ggplot(model6,aes(x=hour, y=totalrides)) + 
  geom_smooth(method='lm')

ggplot(model7,aes(x=hour, y=totalrides)) + 
  geom_smooth(method='lm')

model8 <-Ubertime %>% group_by(day) %>% summarise(totalrides=n(),.groups= 'drop')
model9 <-Lyfttime %>% group_by(day) %>% summarise(totalrides=n(),.groups= 'drop')
ggplot(model8,aes(x=day, y=totalrides)) + 
  geom_smooth(method='lm')

ggplot(model9,aes(x=day, y=totalrides)) + 
  geom_smooth(method='lm')
model <- lm(price ~ hour+day , data = timedata)

moonCount = split(sundata,sundata$moonPhase)
count(moonCount[["0.09"]])
count(moonCount[["0.18"]])
moonPhaseCount <-sapply(moonCount,NROW)

moonPhaseValues <- unique(sundata[c("moonPhase")])
newMoon <-order(moonPhaseValues["moonPhase"])
