#Pterois volitans 

Pv <- gbif("Pterois")
Pv.s <-Pv[Pv$species=='Pterois volitans',]
Pv.1<-subset(Pv.s,select=c("country",'year', 'month', "lat","lon",'species'))
Pteroisvolitans <-Pv.1[Pv.1$country=='United States',]
head(Pteroisvolitans)
write.csv(Pteroisvolitans, file="Pteroisvolitans.csv")
