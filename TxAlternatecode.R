#alternative mapping code for Tamarix 

install.packages("tidyverse")
library(tidyverse)
install.packages('ggmap')
install.packages("osmdata")
library(ggmap)
library(osmdata)
names(TxUSA)

TxUSA$lat <- as.numeric(TxUSA$lat, na.rm =T)
TxUSA$lon <- as.numeric(TxUSA$lon, na.rm =T)

bb <- c(left=min(TxUSA$lon - 0.007, na.rm = T), bottom = min(TxUSA$lat - 0.007, na.rm = T),
        right = max(TxUSA$lon + 0.007, na.rm = T), top = max(TxUSA$lat + 0.007, na.rm = T))

head(TxUSA)
Txlocations <- subset(x = TxUSA,select = c('lat', 'lon'))
head(Txlocations)
attach(Txlocations);
str(Txlocations)
require(raster)
require(rgdal)
Txlocations

Toccurence<- SpatialPoints(Txlocations, proj4string=CRS("+proj=longlat +ellps=WGS84"))

library(rworldmap)
newmap<- getMap(resolution="low")

plot(newmap)
points(Txlocations, pch=16, cex=.5)




#This is from Nics R, but not Chad's
nrow(Txdata.)

require(raster)
require(rgdal)

r <- getData("worldclim",var="bio",res=2.5)
bio <- r[[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]]
names(bio) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11","bio12","bio13","bio14","bio15","bio16","bio17", "bio18","bio19")

values <- extract(bio,locations)

txdata2<-cbind(Txdata.,values)

write.csv(txdata2, file="Txdata2.csv")


