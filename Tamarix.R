#tamarix data
install.packages('dismo')
install.packages("raster")
install.packages('sp')
library('raster')
library("dismo")
library("sp")
Tamarix <- gbif("Tamarix")
head(Tamarix)
names(Tamarix)
location<-subset(Tamarix,select=c("country",'year', 'month', "lat","lon"))
head(location)
TxUSA <- location[location$country=='United States',]
TxUSA

write.csv(TxUSA, file="Tamarix.csv")
