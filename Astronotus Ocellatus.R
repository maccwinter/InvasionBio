#Astronotus ocellatus 
library('raster')
library("sp")
library("dismo")
Astro <- gbif("Astronotus")
names(Astro)
A.ocellatus <-Astro[Astro$species=='Astronotus ocellatus',]
A.ocellatus1<-subset(A.ocellatus,select=c("country",'year', 'month', "lat","lon",'species'))
Astronotusocellatus <-A.ocellatus1[A.ocellatus1$country=='United States',]
head(Astronotusocellatus)
write.csv(Astronotusocellatus, file="Astronotusocellatus.csv")
