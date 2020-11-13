#Amphillopus citrinellus 

library('raster')
library("sp")
library("dismo")
Mcichlid <- gbif("Clarias")
names(Mcichlid)
Batrachus <-Mcichlid[Mcichlid$species=='Clarias batrachus',]
ClariasB<-subset(Batrachus,select=c("country",'year', 'month', "lat","lon",'species'))
Clariasbatrachas <-ClariasB[ClariasB$country=='United States',]
head(Clariasbatrachas)
write.csv(Clariasbatrachas, file="Clariasbatrachus.csv")
