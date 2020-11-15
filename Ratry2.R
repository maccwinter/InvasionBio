#Ra try 2


library('raster')
library("dismo")
library("sp")
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


raloc <- subset(radata,select=c('lon','lat'))
head(raloc)

raloc. <- na.omit(raloc)

head(raloc.)

RAlocations<- SpatialPoints(raloc., proj4string=CRS("+proj=longlat +ellps=WGS84"))

head(locations)

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



