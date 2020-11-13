#Blackfordia virginica 


Bv <- gbif("Blackfordia")
Bv.s <-Bv[Bv$species=='Blackfordia virginica',]
Bv.1<-subset(Bv.s,select=c("country",'year', 'month', "lat","lon",'species'))
Blackfordiavirginica <-Bv.1[Bv.1$country=='United States',]
head(Blackfordiavirginica)
write.csv(Blackfordiavirginica, file="Blackfordiavirginica.csv")
