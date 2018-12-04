
#empty the session
rm(list = ls())

#setting working directory
WD = "C:/CNRS/Cours/R_M2/TP1"
setwd(WD)

#create the 7 bands image
ListBandFiles = list.files(".", pattern = 'Calib.tif', recursive=TRUE, include.dirs=TRUE)
Landsat.im = stack(ListBandFiles)
names(Landsat.im) = c("B","G","R","NIR","MIR1","TIR","MIR2")

plot(subset(Landsat.im, 4))
#cut out the image
NewExt = drawExtent()
Landsat.im = crop(Landsat.im,NewExt)

#converting the image into a matrix (one band = one column)
Landsat.mx = as.matrix(Landsat.im)
dim(Landsat.mx)
head(Landsat.mx)

#ACCA - Pass1
print("Filter1")
Filter1 = (Landsat.mx[,3] > 0.08*255) #below it is non clouds

print("Filter2")
NDSI = (Landsat.mx[,2] - Landsat.mx[,5])/(Landsat.mx[,2] + Landsat.mx[,5])
Filter2 = (NDSI < 0.7) #above it = nonclouds

print("Filter3")
Filter3 = Landsat.mx[,6] < 300-173 #above it = nonclouds

print("Filter4")
Band56comp = (1 - (Landsat.mx[,5]/255))*(Landsat.mx[,6]+173)
Filter4 = (Band56comp < 225) #above it = ambiguous

print("Filter5")
Band43ratio = (Landsat.mx[,4] / Landsat.mx[,3])
Filter5 = Band43ratio < 2 #above is ambiguous

print("Filter6")
Band42ratio = (Landsat.mx[,4] / Landsat.mx[,2])
Filter6 = Band42ratio < 2 #above is ambiguous

print("Filter7")
Band45ratio = (Landsat.mx[,4] / Landsat.mx[,5])
Filter7 = Band45ratio > 1 #below is ambiguous


ACCA.vc = (Filter1 == 1) & (Filter2 == 1) & (Filter3 == 1) & (Filter4 == 1) & (Filter5 == 1) & (Filter6 == 1) & (Filter7 == 1)
unique(ACCA.vc)

#export to image
ACCA.im = subset(Landsat.im, 1)
ACCA.im = setValues(ACCA.im,ACCA.vc)
plot(ACCA.im)



#ESSAYONS DE TRAITER L'IMAGE ENTIERE PAR TUILES
#########################

# Quel intérêt de "tuiler"?
# Parfois l'image et tous les traitements intermédiaires sont trop lourds et ne peuvent pas être traités d'une seule fois (dépend des capacités de votre ordinateur)
# dans ce cas, on peut "tuiler", c'est à dire découper l'image et traiter une tuile à la fois. On recolle les morceaux au fur et à mesure
# 

ListBandFiles = list.files(".", pattern = 'Calib.tif', recursive=TRUE, include.dirs=TRUE)
Landsat.im = stack(ListBandFiles)
names(Landsat.im) = c("B","G","R","NIR","MIR1","TIR","MIR2")

trRasterBlock = blockSize(Landsat.im) #définit les coordonnées de chaque tuile
trRasterBlock


ACCA.all = NULL  #initialisation d'une variable qui recevra les données traitées à chaque tuile

for (i in 1:trRasterBlock$n){
  
  cat("Tuile = ", i, "\n")
  RasterBlock = getValuesBlock(Landsat.im, row=trRasterBlock$row[i], nrows=trRasterBlock$nrows[i])
  
  #ACCA - Pass1
  cat("Filter1 \t")
  Filter1 = (RasterBlock[,3] > 0.08*255) #below it is non clouds
  
  cat("Filter2 \t")
  NDSI = (RasterBlock[,2] - RasterBlock[,5])/(RasterBlock[,2] + RasterBlock[,5])
  Filter2 = (NDSI < 0.7) #above it = nonclouds
  
  cat("Filter3 \t")
  Filter3 = RasterBlock[,6] < 300-173 #above it = nonclouds
  
  cat("Filter4 \t")
  Band56comp = (1 - (RasterBlock[,5]/255))*(RasterBlock[,6]+173)
  Filter4 = (Band56comp < 225) #above it = ambiguous
  
  cat("Filter5 \t")
  Band43ratio = (RasterBlock[,4] / RasterBlock[,3])
  Filter5 = Band43ratio < 2 #above is ambiguous
  
  cat("Filter6 \t")
  Band42ratio = (RasterBlock[,4] / RasterBlock[,2])
  Filter6 = Band42ratio < 2 #above is ambiguous
  
  cat("Filter7 \n")
  Band45ratio = (RasterBlock[,4] / RasterBlock[,5])
  Filter7 = Band45ratio > 1 #below is ambiguous
   
  ACCA.block = (Filter1 == 1) & (Filter2 == 1) & (Filter3 == 1) & (Filter4 == 1) & (Filter5 == 1) & (Filter6 == 1) & (Filter7 == 1)
  ACCA.all = c(ACCA.all,ACCA.block)
}

length(ACCA.all)      

ncell(Landsat.im)
  
#creating the final image:
ACCA.im = subset(Landsat.im, 1)
ACCA.im = setValues(ACCA.im,ACCA.all)
plot(ACCA.im)

#making a function
ACCA = function(WD){
  
  setwd(WD)
  
  ListBandFiles = list.files(WD, pattern = 'Calib.tif', recursive=TRUE, include.dirs=TRUE)
  Landsat.im = stack(ListBandFiles)
  names(Landsat.im) = c("B","G","R","NIR","MIR1","TIR","MIR2")
  
  trRasterBlock = blockSize(Landsat.im) #définit les coordonnées de chaque tuile
#   trRasterBlock
  
  ACCA.all = NULL  #initialisation d'une variable qui recevra les données traitées à chaque tuile
  
  for (i in 1:trRasterBlock$n){
    
    cat("Tuile = ", i, "\n")
    RasterBlock = getValuesBlock(Landsat.im, row=trRasterBlock$row[i], nrows=trRasterBlock$nrows[i])
    
    #ACCA - Pass1
    cat("Filter1 \t")
    Filter1 = (RasterBlock[,3] > 0.08*255) #below it is non clouds
    
    cat("Filter2 \t")
    NDSI = (RasterBlock[,2] - RasterBlock[,5])/(RasterBlock[,2] + RasterBlock[,5])
    Filter2 = (NDSI < 0.7) #above it = nonclouds
    
    cat("Filter3 \t")
    Filter3 = RasterBlock[,6] < 300-173 #above it = nonclouds
    
    cat("Filter4 \t")
    Band56comp = (1 - (RasterBlock[,5]/255))*(RasterBlock[,6]+173)
    Filter4 = (Band56comp < 225) #above it = ambiguous
    
    cat("Filter5 \t")
    Band43ratio = (RasterBlock[,4] / RasterBlock[,3])
    Filter5 = Band43ratio < 2 #above is ambiguous
    
    cat("Filter6 \t")
    Band42ratio = (RasterBlock[,4] / RasterBlock[,2])
    Filter6 = Band42ratio < 2 #above is ambiguous
    
    cat("Filter7 \n")
    Band45ratio = (RasterBlock[,4] / RasterBlock[,5])
    Filter7 = Band45ratio > 1 #below is ambiguous
    
    ACCA.block = (Filter1 == 1) & (Filter2 == 1) & (Filter3 == 1) & (Filter4 == 1) & (Filter5 == 1) & (Filter6 == 1) & (Filter7 == 1)
    ACCA.all = c(ACCA.all,ACCA.block)
  }
  ACCA.im = subset(Landsat.im, 1)
  ACCA.im = setValues(ACCA.im,ACCA.all)
  writeRaster(ACCA.im,"CloudMask.tif",format="GTiff",datatype="INT1U",overwrite=T)
  return(ACCA.im)
}


#appliquer la fonction
system.time({
ACCA.image = ACCA(WD)
})#1.60 mins


#Apply the function to a serie of images
#
#################################################################
WD = "C:/CNRS/Cours/R_M2/TP2"
setwd(WD)

#list tous les sous-dossiers (sachant que un sous-dossier = une image)
List.dirs = dir()[file.info(dir())$isdir]


strt = Sys.time()
for (i in 1:length(List.dirs)){
  WD.tmp = paste(WD,List.dirs[i],sep="/")
  cat(WD.tmp, "\n")
  ACCA(WD.tmp)
}
print(Sys.time()-strt)
#6.83 minutes pour 4 images



#Parallel processing
library(foreach)
library(parallel)
library(doSNOW)


WD = "C:/CNRS/Cours/R_M2/TP2"
setwd(WD)

#list all subfiles (one subfile = one image)
List.dirs = dir()[file.info(dir())$isdir]
List.WD = paste(WD,List.dirs,sep="/")

#Initialize parallel program
strt<-Sys.time()
n.cores <- detectCores()-1
cl<-makeCluster(n.cores)
registerDoSNOW(cl)
cat("\t\t\t Number of cores = ",n.cores, "\n")

#do parallel loop to extract pre-classifications
foreach(k = 1:length(List.WD), .export=c("ACCA"), .packages=c("raster","parallel","doSNOW"),.inorder=F) %dopar% {
  ACCA(List.WD[k])                  
}
#Close cluster
stopCluster(cl)

print(Sys.time()-strt)
#4.19 minutes pour 4 images

#La différence (+ de 2 minutes gagnées) est intéressante mais pas extraordinaire MAIS...
#en supposant qu'on est un serveur avec 24 processeurs (cas du labo COSTEL)
# méthode 1 = 24*(6.83/4) = 40.98 mins pour traiter 24 images
# méthode 2 = environ 5 minutes 
