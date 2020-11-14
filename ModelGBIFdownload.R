#This is the model of how to best download data from gbif!! 

#Rubus 

library('raster')
library("dismo")
library("sp")


Ra. <- gbif("Rubus",species ='armeniacus')

head(Ra.)

Ra.s<-subset(Ra.,select=c("country",'year', 'month', "lat","lon",'species'))

Rubusarmeniacus <-Ra.s[Ra.s$country=='United States',]

head(Rubusarmeniacus)
write.csv(Rubusarmeniacus, file="Rubusarmeniacus.csv")
