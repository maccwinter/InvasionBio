#Official

#Tamarix ------Done 
library(dismo)
Species <- gbif("Tamarix")
SPUSA <- Species[Species$country=='United States',]
write.csv(SPUSA, file="Tamarix.csv")

read.csv("Tamarixspp.csv")-> cidata;
attach(cidata);
str(cidata)
require(raster)
require(rgdal)
#Rubus armeniacus --------------------

library(dismo)
Species <- gbif("Rubus", 'armeniacus')
SPUSA <- Species[Species$country=='United States',]
write.csv(SPUSA, file="Rubus_armeniacus.csv")

read.csv("Rubus_armeniacus.csv")-> cidata; 
attach(cidata);
str(cidata)
require(raster)
require(rgdal)

#Clarias batrachus ---- 
library(dismo)
Species <- gbif("Clarias", 'batrachus')
SPUSA <- Species[Species$country=='United States',]
write.csv(SPUSA, file="Clarias_batrachus.csv")

read.csv("Clarias_batrachus.csv")-> cidata; 
attach(cidata);
str(cidata)
require(raster)
require(rgdal)


#Astronotus ocellatus ----

library(dismo)
Species <- gbif("Astronotus", 'ocellatus')
SPUSA <- Species[Species$country=='United States',]
write.csv(SPUSA, file="Astronotus_ocellatus.csv")

read.csv("Astronotus_ocellatus.csv")-> cidata; 
attach(cidata);
str(cidata)
require(raster)
require(rgdal)
#---------------------------

#Blackfordia virginiaca------

library(dismo)
Species <- gbif("Blackfordia", 'virginiaca')
SPUSA <- Species[Species$country=='United States',]
write.csv(SPUSA, file="Blackfordia_virginiaca.csv")

read.csv("Blackfordia_virginica.csv")-> cidata; 
attach(cidata);
str(cidata)
require(raster)
require(rgdal)



#Chorisporatenella-----

library(dismo)
Species <- gbif("Chorispora", 'tenella')
SPUSA <- Species[Species$country=='United States',]
write.csv(SPUSA, file="Chorispora_tenella.csv")

read.csv("Chorispora_tenella.csv")-> radata; 
attach(cidata);
str(cidata)
require(raster)
require(rgdal)



#Monopterusalbus-----

library(dismo)
Species <- gbif("Monopterus", 'albus')
SPUSA <- Species[Species$country=='United States',]
write.csv(SPUSA, file="Monopterus_albus.csv")

read.csv("Monopterus_albus.csv")-> cidata; 
attach(cidata);
str(cidata)
require(raster)
require(rgdal)


#Pteroisvolitans-----

library(dismo)
Species <- gbif("Pterois", 'volitans')
SPUSA <- Species[Species$country=='United States',]
write.csv(SPUSA, file="Pterois_volitans.csv")

read.csv("Pterois_volitans.csv")-> radata; 
attach(cidata);
str(cidata)
require(raster)
require(rgdal)

#here is where you proceed after reading in gbif data-------

year.lon.lat <- subset(cidata,select=c('year','lon','lat'))

year.lon.lat.filtered <- na.omit(year.lon.lat)


cidata <-subset(year.lon.lat.filtered, select=c('year','lon','lat'))

locations<- SpatialPoints(cidata[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))

library(dplyr)
introyears <-cidata[order(cidata$year),]
introyear<- SpatialPoints(introyears[1,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))


#Create initial map to make sure we only have native and invasive populations
#download raster file for map of the world
library(rworldmap)
newmap<- getMap(resolution="low")



#Extract bioclim values
#Load libraries necessary for working with raster datasets
require(raster)
require(rgdal)

# bioclim values -----------------------
r <- getData("worldclim",var="bio",res=2.5)
bio <- r[[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]]
names(bio) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11","bio12","bio13","bio14","bio15","bio16","bio17", "bio18","bio19")

values <- extract(bio,locations)

#you can print these out
cidata2<-cbind(cidata,values)
write.csv(cidata2, file='Txclimdata.csv')

#Or load if you already have them downloaded ------
bio1<- raster('wc2-5/bio1.bil')
bio2<- raster('wc2-5/bio2.bil')
bio3<- raster('wc2-5/bio3.bil')
bio4<- raster('wc2-5/bio4.bil')
bio5<- raster('wc2-5/bio5.bil')
bio6<- raster('wc2-5/bio6.bil')
bio7<- raster('wc2-5/bio7.bil')
bio8<- raster('wc2-5/bio8.bil')
bio9<- raster('wc2-5/bio9.bil')
bio10<- raster('wc2-5/bio10.bil')
bio11<- raster('wc2-5/bio11.bil')
bio12<- raster('wc2-5/bio12.bil')
bio13<- raster('wc2-5/bio13.bil')
bio14<- raster('wc2-5/bio14.bil')
bio15<- raster('wc2-5/bio15.bil')
bio16<- raster('wc2-5/bio16.bil')
bio17<- raster('wc2-5/bio17.bil')
bio18<- raster('wc2-5/bio18.bil')
bio19<- raster('wc2-5/bio19.bil')

#bio <- cbind(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11,bio12,bio13,bio14,bio15,bio16,bio17, bio18,bio19)

#make a prettier map

#bio5 = Max temperature of warmest month
#bio10 = Mean temperature of warmest quarter
#bio11 = Mean temperature of coldest quarter
#bio12 = Total (annual) precipitation
#bio14 =precip driest motth





#Okay, the important data to generate is the 

#get the distance between two geographic points
library(geosphere)

library(dplyr)
introyear <-cidata[order(cidata$year),]
head(introyear)

introyear[1,2:3]

IRlocations <- SpatialPoints(cidata[,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
IRinitiallocations<-SpatialPoints(introyear[1,2:3], proj4string=CRS("+proj=longlat +ellps=WGS84"))
dist<-distHaversine(IRlocations , IRinitiallocations)
head(dist)

InvasiveRange_new<-cbind(cidata,dist)

#Lets plot it - what problem do you see here?
plot(InvasiveRange_new$year, (InvasiveRange_new$dist/1000), pch=16,main ='R.armeniacus', xlab="Years since Introduction", ylab="Distance from Introduced population (km)")


#get maximum distance from starting point
max_values2<- InvasiveRange_new %>% group_by(year) %>% slice(which.max(dist))
#must make into a dataframe to use in dpylr again
maxvalues2<- as.data.frame(max_values2)

#Get the culmulative maximum values
culmmax<-mutate(maxvalues2,culmaxdist= cummax(dist))

#Make into a dataframe again

culmmax2<-as.data.frame(culmmax)
#use mutate to get the slope at each point
diffs<-mutate(culmmax2, D_delta = (((culmaxdist/1000) - lag(culmaxdist/1000))/(year-lag(year))))
diffs
diffs$D_delta

diffs$dist/1000
plot(diffs$year, diffs$D_delta, pch=16,main ='Tamarix Invasion Velocity', xlab="Year", ylab="Velocity of Invasion (km/year)")

#add a loess regression
loessMod60 <- loess((culmmax$culmaxdist/1000) ~ culmmax$year, data=culmmax, span=0.6) 
smoothed60 <- predict(loessMod60) 
lines(culmmax$year,smoothed60, col="blue", lwd=2)

#max dist ----- 
furthest<- InvasiveRange_new[desc(InvasiveRange_new$dist),]
top <-head(furthest, n=10)
topyear <-subset(top, select ='year')
topyear
location.max.dist <- subset(top, select=c('lon','lat'))

#at point where speed slows.# ----- 
Yearvelocity <-diffs[desc(diffs$D_delta),] 
topvelocity <-head(Yearvelocity, n=1)
topyearvelocity <-subset(topvelocity, select ='year')
loc.velocity.slows <- subset(topvelocity[2,], select=c('lon','lat'))
#shall I add this to maps? 
#I want to map points after max velocity 
#maxvelocity: = 
postmvelocity =subset(cidata2[cidata2$year>=topyearvelocity,], select =c('lon','lat','bio5','bio6','bio12','bio14'))

#maps final-----------------------------------
library(rworldmap)
newmap <- getMap(resolution = 'low')
name ="Tamarix.spp"

# total precipitation-----

plot((bio12/10), xlim=c(-160,-50), ylim=c(10,85))
points(locations$lon,locations$lat, col ='red',pch=16, cex=0.25)
points(postmvelocity$lon, postmvelocity$lat, col ='green',pch=16, cex=0.25)
points(location.max.dist$lon, location.max.dist$lat, col ='black', pch=16, cex=0.25)
points(introyear[1,2], introyear[1,3], col ='blue', pch=16, cex=0.25)
title(main = name, xlab = 'total precip (cm/year)')
addMapLegendBoxes(x='bottomleft', title ='Location type' ,cex =0.35, pt.cex=1, colourVector = c('blue','red','black','green'), legendText =c ("original location",'total','furthest distance','post max spread velocity'))

#max temperature ----
map_tempmax <-plot(bio5/10, xlim=c(-160,-50), ylim=c(10,85))
points(locations$lon,locations$lat, col ='red',pch=16, cex=0.25)
points(postmvelocity$lon, postmvelocity$lat, col ='green',pch=16, cex=0.25)
points(location.max.dist$lon, location.max.dist$lat, col ='black', pch=16, cex=0.25)
points(introyear[1,2], introyear[1,3], col ='blue', pch=16, cex=0.25)
title(main = name, xlab = 'max temp (C)')
addMapLegendBoxes(x='bottomleft', title ='Location type' ,cex =0.35, pt.cex=1, colourVector = c('blue','red','black','green'), legendText =c ("original location",'total','furthest distance','post max spread velocity'))


#min temp ----
plot((bio6/10), xlim=c(-160,-50), ylim=c(10,85))
points(locations$lon,locations$lat, col ='red',pch=16, cex=0.25)
points(postmvelocity$lon, postmvelocity$lat, col ='green',pch=16, cex=0.25)
points(location.max.dist$lon, location.max.dist$lat, col ='black', pch=16, cex=0.25)
points(introyear[1,2], introyear[1,3], col ='blue', pch=16, cex=0.25)
title(main = name, xlab = 'min temp (C)')
addMapLegendBoxes(x='bottomleft', title ='Location type' ,cex =0.35, pt.cex=1, colourVector = c('blue','red','black','green'), legendText =c ("original location",'total','furthest distance','post max spread velocity'))


#lowest precip month ----
plot((bio6/10), xlim=c(-160,-50), ylim=c(10,85))
points(locations$lon,locations$lat, col ='red',pch=16, cex=0.25)
points(postmvelocity$lon, postmvelocity$lat, col ='green',pch=16, cex=0.25)
points(location.max.dist$lon, location.max.dist$lat, col ='black', pch=16, cex=0.25)
points(introyear[1,2], introyear[1,3], col ='blue', pch=16, cex=0.25)
title(main = name, xlab = 'lowest precip (cm/month)')
addMapLegendBoxes(x='bottomleft', title ='Location type' ,cex =0.35, pt.cex=1, colourVector = c('blue','red','black','green'), legendText =c ("original location",'total','furthest distance','post max spread velocity'))

#Is spread being contained? 

furthest<- InvasiveRange_new[desc(InvasiveRange_new$dist),]
top <-head(furthest, n=10)
topyear <-subset(top, select ='year')
topyear
location.max.dist <- subset(top, select=c('lon','lat'))
library(rworldmap)
newmap <- getMap(resolution = 'low')

#at furthest distance and original -------
#bio5 = Max temperature of warmest month
#bio6 = low temperature of coldest quarter
#bio12 = Total (annual) precipitation
#bio14 =precip driest motth



# Old map codes (just in case)------

plot((bio12/100), xlim=c(-160,-50), ylim=c(10,85))
points(location.max.dist$lon, location.max.dist$lat, col ='dark red', pch=16, cex=3)
points(introyear[1,2], introyear[1,3], col ='blue', pch=16, cex=3)
title(main = name, xlab = 'blue:original red:furthest', ylab = 'Total precipitation (cm)')
plot(newmap, add=T)

#max temperature ----
plot(bio5, xlim=c(-160,-50), ylim=c(10,85))
points(location.max.dist$lon, location.max.dist$lat, col ='red', pch=16, cex=3)
title(main = name, xlab = 'blue:original red:furthest', ylab = 'max temp C')
plot(newmap, add=T)

#min temp ----
plot((bio6/100), xlim=c(-160,-50), ylim=c(10,85))
points(location.max.dist$lon, location.max.dist$lat, col ='red', pch=16, cex=3)
points(introyear[1,2], introyear[1,3], col ='blue', pch=16, cex=3)
title(main =name, xlab = 'blue:original red:furthest ', ylab = 'Min Temp C')
plot(newmap, add=T)

#lowest precip month ----
plot((bio14/100), xlim=c(-160,-50), ylim=c(10,85))
points(location.max.dist$lon, location.max.dist$lat, col ='red', pch=16, cex=3)
points(introyear[1,2], introyear[1,3], col ='blue', pch=16, cex=3)
title(main =name, xlab = 'blue:original red:furthest', ylab = 'Precipitation_lowest (cm/year)')
plot(newmap, add=T)



furthest<- diffs[desc(diffs$D_delta),]
top <-head(furthest, n=10)
topyear <-subset(top, select ='year')
topyear
location.max.dist <- subset(top, select=c('lon','lat'))
