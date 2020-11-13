#Monopterus albus 

Ma <- gbif("Monopterus")
Ma.s <-Ma[Ma$species=='Monopterus albus',]
Ma.1<-subset(Ma.s,select=c("country",'year', 'month', "lat","lon",'species'))
Monopterusalbus <-Ma.1[Ma.1$country=='United States',]
head(Monopterusalbus)
write.csv(Monopterusalbus, file="Monopterusalbus.csv")
