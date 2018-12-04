#
# OBJECTIFS
#############################################################################################

# process time series of precipitation
# change the temporal resolution of the data
# Analyze data at certain points / city
# Mapping the rainiest month / the driest month
# 
############################################################################################

rm(list=(ls()))

library(raster)
library(ncdf)



WD = "C:/CNRS/Cours/R_M2/TP3/TRMM"
setwd(WD)


#
#process time series of precipitation
######################################################################################

ListTRMMFiles = list.files(".", pattern = '.nc', recursive=TRUE, include.dirs=TRUE)
length(ListTRMMFiles) #on a 366 images (un an - 2012)

image.trmm = raster(ListTRMMFiles[1])


NewExt = drawExtent()
plot(crop(image.trmm,NewExt))


#extract the daily time series for a given point
#Lat = -15, long = -65
Lat = - 15
Lon = 295

#quelle cellule correspond à cette coordonnée?
CellNumber = cellFromXY(image.trmm,c(Lon,Lat)) #on inverse Lat Lon car on indique x Y

#créer de la base de données:
TRMM.stack = stack(ListTRMMFiles)

#extraire la série temporelle
TimeSerie.cell = TRMM.stack[CellNumber]
#TimeSerie.cell est une matrice d'une seule ligne... 
plot(TimeSerie.cell[1,],type="l", main = "Time serie", ylab = "Pmm", xlab = "Julian day")



#changing temporal resolution of the data 
##############################################################
#précipitations annuelles:
#########################
system.time({
Trmm.annuel = sum(TRMM.stack)
plot(crop(Trmm.annuel,NewExt))
})#33.24 secs

#autre approche possible (moins intuitive mais plus rapide et qui ouvre plus de possibilités pour d'autres calculs):
library(matrixStats)
system.time({
Trmm.mx = as.matrix(TRMM.stack)
Trmm.sum = rowSums(Trmm.mx,na.rm=T)
Trmm.annuel2 = setValues(image.trmm,Trmm.sum)
plot(crop(Trmm.annuel2,NewExt))
})#20.61 secs


#précipitations mensuelles
##########################
#extraire les dates à partir des fichiers
#we want an image with 12 bands (one band = one month)
Trmm.dates.split = strsplit(ListTRMMFiles,"[.]")

Trmm.year = sapply(Trmm.dates.split, "[[", 2)
Trmm.month = sapply(Trmm.dates.split, "[[", 3)
Trmm.day = sapply(Trmm.dates.split, "[[", 4)

UniqueMonths = unique(Trmm.month)
Trmm.month.all = NULL
for (i in 1:length(UniqueMonths)){
    ind.months = which(Trmm.month == UniqueMonths[i],arr.ind=T)
    cat("Year = ", Trmm.year[ind.months[1]]," - Month = ", UniqueMonths[i], "\n" )
    Trmm.month.vc = rowSums(Trmm.mx[,ind.months],na.rm=T)
    Trmm.month.im = setValues(image.trmm,Trmm.month.vc)
    if (i == 1){
      Trmm.month.all = Trmm.month.im
    }else{  
      Trmm.month.all = stack(Trmm.month.all,Trmm.month.im)
    }
}
plot(Trmm.month.all)
plot(crop(Trmm.month.all,NewExt))

#extract the monthly time series
TimeSerie.cell.month = Trmm.month.all[CellNumber]
#TimeSerie.cell est une matrice d'une seule ligne... 
barplot(TimeSerie.cell.month[1,], main = "Time serie", ylab = "Pmm", xlab = "Month", names.arg = UniqueMonths)


#Extract time series for the world's largest cities
##############################################################
#opn shapefiles
library(sp)
library(rgeos)
library(rgdal)
WD = "C:/CNRS/Cours/R_M2/TP3"
setwd(WD)
Cities.shp = readOGR(dsn=".",layer = "ne_10m_populated_places_simple")
head(Cities.shp)
names(Cities.shp)
head(Cities.shp@data)

#most populated city?
city.big= Cities.shp[Cities.shp$pop_max == max(Cities.shp$pop_max),]
city.big$name
city.big$pop_max


#projection ofCities.shp?
Proj.cities = projection(Cities.shp) 


#projection of TRMM data?
Proj.trmm = projection(image.trmm)


#same projection
plot(Trmm.annuel)
plot(Cities.shp,add=T)

Trmm.annuel = rotate(Trmm.annuel)
plot(Trmm.annuel)
plot(Cities.shp,add=T)

Trmm.month.all = rotate(Trmm.month.all)
# TRMM.stack = rotate(TRMM.stack)


#select cities of more than 10 millions inhabitants
city.10M = Cities.shp[Cities.shp$pop_max > 10000000 & Cities.shp$latitude < 50 & Cities.shp$latitude > -50,]
city.10M$name
plot(Trmm.annuel)
plot(city.10M,add=T)

#extract the 2012 monthly precipitation for these cities
Trmm.city.10M = extract(Trmm.month.all,city.10M)
colnames(Trmm.city.10M) = UniqueMonths

#plot
par(mfrow = c(5,4))
for (i in 1:length(city.10M)){
  barplot(Trmm.city.10M[i,], main = city.10M$name[i], ylab = "Pmm", xlab = "Month", names.arg = UniqueMonths)
}

#same scale everywhere
Max.pmm = max(Trmm.city.10M)
par(mfrow = c(5,4))
for (i in 1:length(city.10M)){
  barplot(Trmm.city.10M[i,], main = city.10M$name[i], ylab = "Pmm", xlab = "Month", names.arg = UniqueMonths, ylim = c(0,Max.pmm))
}

par(mfrow = c(5,4))
for (i in 1:length(city.10M)){
  barplot(Trmm.city.10M[i,], main = city.10M$name[i], ylab = "Pmm", xlab = "Month", names.arg = UniqueMonths, ylim = c(0,300))
}



#Mapping the rainiest month / the driest month
##############################################################


Trmm.month.mx = as.matrix(Trmm.month.all)
Trmm.month.max = apply(Trmm.month.mx,1,which.max) #pluvieux
Trmm.month.min = apply(Trmm.month.mx,1,which.min) #sec

#export as image
Trmm.month.max.im = setValues(rotate(image.trmm),Trmm.month.max)
Trmm.month.min.im = setValues(rotate(image.trmm),Trmm.month.min)

par(mfrow = c(2,1))
plot(Trmm.month.max.im, main = "Mois le plus pluvieux")
plot(Trmm.month.min.im, main = "Mois le plus sec")
# image(Trmm.month.max.im)
# image(Trmm.month.min.im)
