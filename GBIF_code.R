#Cleaned code for downloading GBIF and climate data. 


library(raster)
library(sp)
library(rgdal)
library(dismo)
species <- gbif("Rubus", 'armeniacus', geo=TRUE)
spUSA <- species[species$country=='United States',]
year.lon.lat <- subset(spUSA,select=c('year','lon','lat'))
locations<- SpatialPoints(year.lon.lat[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))