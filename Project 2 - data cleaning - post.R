### Some useful packages

library(leaflet)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(igraph)
library(data.table)
library(maps)
library(DT)
library(ggthemes)
library(geosphere)

#################################Data Step for combining all downloaded file#############################
# Initialize an empty data set
# Only load data from 2016
setwd("C:\\Users\\longx\\Downloads\\2006")
data2006 <- NULL
for(year in 2006:2006){
  for(month in 1:12){
    # read in data name
    temp_data <- fread(paste0("On_Time_On_Time_Performance_",year,"_",month,".csv"))
    # clean data in some standarized way
    
    # combine with other month data
    data2006 <- rbind(data2006, temp_data)
  }
}
########################################################################################################
reduced2006<-data2006%>%
  select(myVars)
save(reduced2006, file = "reduced2006.rda")
## some useful vectors
# Choose some variables of interst
myVars <- c("Year","Month","DayofMonth","DayOfWeek","UniqueCarrier" ,"DayOfWeek","FlightDate","Carrier",
            "Origin","OriginCityName","OriginState","OriginStateName",
            "Dest", "DestCityName", "DestState", "DestStateName","AirTime",
            "Distance", "ArrDelay", "ArrDelayMinutes", "DepDelay","DepDelayMinutes" ,
            "Cancelled", "CancellationCode", "Diverted","CarrierDelay", "WeatherDelay", "NASDelay", 
            "SecurityDelay", "LateAircraftDelay")


# Load downloaded data
#setwd("/Users/nguyens/Google Drive/Classes/STA 504/Data/BTS - Airlines On-time Performance/2016")
setwd("C:\\Users\\longx\\Google Drive\\Classes\\STA 504\\Data\\BTS - Airlines On-time Performance\\2016")
load("reduced2006.rda")
airportList1 <- read.csv("airportListLatLong.csv")
airportList2 <- read.csv("airportListLatLong2.csv")


### Data for delay mapping
origin <- airportList2$Origin
as.factor(delays_by_origin$Origin)


### Choose variables of interest
delays_by_origin <- reduced2006%>%
  select(Year, Month, OriginCityName, OriginState, Origin,
         CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)%>%
  na.omit()

#Figure out airport volume (delays_by_origin is over-written here)
delays_by_origin <- delays_by_origin%>%
  group_by(Origin)%>%
  mutate(vol = n())

# Carrier delay set
total_carrierDelay<- delays_by_origin%>%
  group_by(Month, Origin, OriginCityName, OriginState,vol, CarrierDelay)%>%
  summarise(total_flights=n())%>%
  mutate(totalDelay = total_flights*CarrierDelay)%>%
  mutate(delay.c=sum(totalDelay),n.c=sum(total_flights))%>%
  select(Month, Origin, OriginCityName, OriginState,delay.c, n.c)%>%
  na.omit()

#Weather delay set
total_weatherDelay<- delays_by_origin%>%
  group_by(Month, Origin, OriginCityName, OriginState,vol, WeatherDelay)%>%
  summarise(total_flights=n())%>%
  mutate(totalDelay = total_flights*WeatherDelay)%>%
  mutate(delay.w=sum(totalDelay),n.w=sum(total_flights))%>%
  select(Month, Origin, OriginCityName, OriginState,delay.w, n.w)%>%
  na.omit()

#NAS delay set
total_NASDelay<- delays_by_origin%>%
  group_by(Month, Origin, OriginCityName, OriginState,vol, NASDelay)%>%
  summarise(total_flights=n())%>%
  mutate(totalDelay = total_flights*NASDelay)%>%
  mutate(delay.n=sum(totalDelay),n.n=sum(total_flights))%>%
  select(Month, Origin, OriginCityName, OriginState,delay.n, n.n)%>%
  na.omit()

#Security delay set
total_securityDelay<- delays_by_origin%>%
  group_by(Month, Origin, OriginCityName, OriginState,vol, SecurityDelay)%>%
  summarise(total_flights=n())%>%
  mutate(totalDelay = total_flights*SecurityDelay)%>%
  mutate(delay.s=sum(totalDelay),n.s=sum(total_flights))%>%
  select(Month, Origin, OriginCityName, OriginState,delay.s, n.s)%>%
  na.omit()

#late aircraft delay set
total_lateAircraftDelay<- delays_by_origin%>%
  group_by(Month, Origin, OriginCityName, OriginState,vol, LateAircraftDelay)%>%
  summarise(total_flights=n())%>%
  mutate(totalDelay = total_flights*LateAircraftDelay)%>%
  mutate(delay.l=sum(totalDelay),n.l=sum(total_flights))%>%
  select(Month, Origin, OriginCityName, OriginState,delay.l, n.l)%>%
  na.omit()

#getting rid of duplicate rows
a<-total_carrierDelay[!duplicated(total_carrierDelay),]
b<-total_weatherDelay[!duplicated(total_weatherDelay),]
c<-total_NASDelay[!duplicated(total_NASDelay),]
d<-total_securityDelay[!duplicated(total_securityDelay),]
e<-total_lateAircraftDelay[!duplicated(total_lateAircraftDelay),]

#merge all delay sets
full_delay <- merge(a, b, by=c("Origin","Month", "OriginCityName", "OriginState", "vol"))
full_delay <- merge(full_delay, c,by=c("Origin","Month", "OriginCityName", "OriginState","vol"))
full_delay <- merge(full_delay, d,by=c("Origin","Month", "OriginCityName", "OriginState","vol"))
full_delay <- merge(full_delay, e,by=c("Origin","Month", "OriginCityName", "OriginState","vol"))

#calculate average delay
full_delay$delay<- rowSums(full_delay[,c(6,8,10,12,14)], na.rm=TRUE)
full_delay$totalDelay<- rowSums(full_delay[,c(7,9,11,13,15)], na.rm=TRUE)
full_delay$avg_delay<- (full_delay$delay/full_delay$totalDelay)
#full_delay <- full_delay[,-c(7,9,11,13,15)]
cols <- c("Origin", "Month", "city", "state", "vol","Carrier Delay","carrier delay", "Weather Delay", "weather delay",
          "NAS Delay","NAS delay", "Security Delay","security delay","Late Aircraft Delay", "late aircraft delay",
          "Total Delay Time", "Number of Delayed Flights", "Average Delay Time" )
colnames(full_delay)<-cols

longestDelayDF <- full_delay[,c(6,8,10,12,14)]
full_delay$longestDelayReason<-colnames(longestDelayDF)[apply(longestDelayDF,1,which.max)]

mostFrequentDelayDF <- full_delay[,c(7,9,11,13,15)]
full_delay$mostFrequentDelayReason <- colnames(mostFrequentDelayDF)[apply(longestDelayDF,1,which.max)]

#Choose airports with lat-long
origin1 <-as.character(unique(airportList2$Origin))
origin2<-as.character(unique(full_delay$Origin))
origin_vec <- intersect(origin1, origin2)

#Choose airports with lat-long
full_delay2 <- full_delay%>%
  filter(Origin %in% origin_vec)

#select variable of interst again
full_delay3<-merge(full_delay2, airportList2, by="Origin")
full_delay4<-full_delay3[!duplicated(full_delay3),]
full_delay4 <- full_delay4%>%
  select(Origin, Month, `Average Delay Time`, vol, airport, city.x, state.x, lat, long,longestDelayReason, mostFrequentDelayReason)

#save data progress
year <- rep("2006", length(full_delay4$Month))
delays2006 <- cbind(year, full_delay4)
cols <- c("Year", "iata", "Month", "average_delay", "vol", "airport","city", "state", 
          "lat", "long", "longestDelayReason", "mostFrequentDelayReason" )
colnames(delays2006)<-cols
save(delays2006, file = "delays2006.rda")


#########################END#######################################################################

