#Ra try 2


library(raster)
library(dismo)
library(sp)
library(ggmap)


Ra. <- gbif("Rubus",species ='armeniacus')

head(Ra.)


Rubusarmeniacus <-Ra.[Ra.$country=='United States',]

head(Rubusarmeniacus)
write.csv(Rubusarmeniacus, file="Rubusarmeniacus1.csv")

read.csv("Rubusarmeniacus1.csv")-> radata; 
attach(radata);
str(radata)
require(raster)
require(rgdal)
head(radata)


raloc <- subset(radata,select=c('year','lon','lat'))
head(raloc)

raloc. <- na.omit(raloc)



RAloc <-subset(raloc., select=c('lon','lat'))

RAlocations<- SpatialPoints(RAloc, proj4string=CRS("+proj=longlat +ellps=WGS84"))


install.packages("rworldmap")
library(rworldmap)
newmap<- getMap(resolution="low")

plot(newmap)
points(locations, pch=16, cex=.5)

install.packages('geosphere')
library(geosphere)

names(radata)

radata$year
radatai<-radata[order(radata$year),]
head(radatai$year)

rainitial<- subset(radatai, select = c('year', 'lon', 'lat'))

head(rainitial)

ra.i <- subset(radatai, radatai$year =='1920', select=c('lon', 'lat'))


RAinitial<-SpatialPoints(ra.i, proj4string=CRS("+proj=longlat +ellps=WGS84"))
dist<-distHaversine(RAlocations, RAinitial)
head(dist)

RAinvasive<-cbind(raloc.,dist)

head(RAinvasive)
#yay finally time to plot spread of invader
??plot
Rubus_amerniacus_invasion <-plot(RAinvasive$year, (RAinvasive$dist/1000), pch=16,main ='Rubus amerniacus invasion from origin', xlab="Year", ylab="Distance from Introduced population (km)")

#Mac you need to save this plot.. I think. Im gonna take a break

#Yay I made a beautiful plot! 

library(dplyr)
#get maximum distance from starting point
max_values2<- RAinvasive %>% group_by(year) %>% slice(which.max(dist))
#must make into a dataframe to use in dpylr again
maxvalues2<- as.data.frame(max_values2)

#Get the culmulative maximum values
culmmax<-mutate(maxvalues2,culmaxdist= cummax(dist))

#Make into a dataframe again

culmmax2<-as.data.frame(culmmax)
#use mutate to get the slope at each point
diffs<-mutate(culmmax2, D_delta = (((culmaxdist/1000) - lag(culmaxdist/1000))/(year-lag(year))))

diffs$D_delta

diffs$dist/1000
plot(diffs$year, diffs$D_delta, pch=16, xlab="Year", ylab="Velocity of Invasion (km/year)")

#add a loess regression
loessMod60 <- loess((culmmax$culmaxdist/1000) ~ culmmax$year, data=culmmax, span=0.6) 
smoothed60 <- predict(loessMod60) 
lines(culmmax$year,smoothed60, col="blue", lwd=2)

##Code for AVG Velocity, Max Velocity, Time to 

summary(diffs$year, diffs$D_delta)

#to get mean & max velocity
summary(diffs$D_delta)

summary(diffs$dist/1000)

summary(culmmax$culmaxdist/1000)

#To find years to max velocity, enter 'diffs' find year that matches up with the max velocty 
diffs

RA.yearstomaxV <- 1983 -1920
RA.yearstomaxV
#years to max velocity = 63

#Now calculating polygons and what not 

RA_1920 <-raloc.[raloc.$year <= 1920,]
RA_1930 <-raloc.[raloc.$year <= 1930,]
RA_1940 <-raloc.[raloc.$year <= 1940,]
RA_1950 <-raloc.[raloc.$year <= 1950,]
RA_1960 <-raloc.[raloc.$year <= 1960,]
RA_1970 <-raloc.[raloc.$year <= 1970,]
RA_1980 <-raloc.[raloc.$year <= 1980,]
RA_1990 <-raloc.[raloc.$year <= 1990,]
RA_2000 <-raloc.[raloc.$year <= 2000,]
RA_2010 <-raloc.[raloc.$year <= 2010,]
RA_2020 <-raloc.[raloc.$year <= 2020,]

RA_1920.sp<-SpatialPoints(RA_1920[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
RA_1930.sp<-SpatialPoints(RA_1930[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
RA_1940.sp<-SpatialPoints(RA_1940[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
RA_1950.sp<-SpatialPoints(RA_1950[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
RA_1960.sp<-SpatialPoints(RA_1960[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
RA_1970.sp<-SpatialPoints(RA_1970[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
RA_1980.sp<-SpatialPoints(RA_1980[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
RA_1990.sp<-SpatialPoints(RA_1990[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
RA_2000.sp<-SpatialPoints(RA_2000[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
RA_2010.sp<-SpatialPoints(RA_2010[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
RA_2020.sp<-SpatialPoints(RA_2020[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))

install.packages("adehabitatHR")
library(adehabitatHR)

RA_1920_poly<-mcp(RA_1920.sp, percent=100, unin = "km", unout = "km2")
RA_1930_poly<-mcp(RA_1930.sp, percent=100, unin = "km", unout = "km2")
RA_1940_poly<-mcp(RA_1940.sp, percent=100, unin = "km", unout = "km2")
RA_1950_poly<-mcp(RA_1950.sp, percent=100, unin = "km", unout = "km2")
RA_1960_poly<-mcp(RA_1960.sp, percent=100, unin = "km", unout = "km2")
RA_1970_poly<-mcp(RA_1970.sp, percent=100, unin = "km", unout = "km2")
RA_1980_poly<-mcp(RA_1980.sp, percent=100, unin = "km", unout = "km2")
RA_1990_poly<-mcp(RA_1990.sp, percent=100, unin = "km", unout = "km2")
RA_2000_poly<-mcp(RA_2000.sp, percent=100, unin = "km", unout = "km2")
RA_2010_poly<-mcp(RA_2010.sp, percent=100, unin = "km", unout = "km2")
RA_2020_poly<-mcp(RA_2020.sp, percent=100, unin = "km", unout = "km2")



