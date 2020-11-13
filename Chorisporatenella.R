#Chorispora tenella 

Ct <- gbif("Chorispora")
Ct.s <-Ct[Ct$species=='Chorispora tenella',]
Ct.1<-subset(Ct.s,select=c("country",'year', 'month', "lat","lon",'species'))
Chorisporatenella <-Ct.1[Ct.1$country=='United States',]
head(Chorisporatenella)
write.csv(Chorisporatenella, file="Chorisporatenella.csv")
