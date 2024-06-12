
library(devtools)

# install_github("Maritimes/Mar.datawrangling")
# install_github("Maritimes/Mar.fleets")

library(Mar.datawrangling)
#library(Mar.fleets)
library(tidyverse)
library(SpatialHub)
library(RColorBrewer)

library(PBSmapping)
library(sf)
library(marmap)
library(geosphere)

#install_github("BradHubley/SpatialHub")
source(file.path(getwd(), "directories.r"))
#datadir="C:/Users/hubleyb/Documents/Halibut/data"
#wd="C:/Users/hubleyb/Documents/Halibut/Git/DataInputs/"
#datadir="C:/Users/harperd/Documents/Halibut/RDataVault"
#wd="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/"

source(file.path(wd, "passwords.r"))
#ds_all <- Mar.datawrangling::load_datasources()
devtools::load_all(".")



### NEW RECEIVER ARRAY



NAFO <- sf::st_read(file.path(datadir,"Mapping","NAFODivisions","Divisions.shp"))

LC <- sf::st_read("R:/Science/Population Ecology Division/Shared/Atlantic Halibut/Mapping/Laurentian Channel MPA/LaurentianChannel_FromRegsCoordinates.shp")
Lophelia <- sf::st_read("R:/Science/Population Ecology Division/Shared/Atlantic Halibut/Mapping/Official Area Closures from Oceans/Lophelia Coral Conseravtion Area/Lophelia_CCA.shp")
EasternCanyons <- sf::st_read("R:/Science/Population Ecology Division/Shared/Atlantic Halibut/Mapping/Official Area Closures from Oceans/Eastern Canyons Conservation Area/EasternCanyons_BoundaryPoly_20220120.shp")

readGEBCO.bathy(file.path(datadir,"Mapping","GEBCO","gebco_2023_AtlCan.nc"))-> GEBCOdata


NOAAdata <- getNOAA.bathy(lon1 = -60, lon2 = -47,lat1 = 40, lat2 = 49, resolution = 0.5)
GEBCOdata <- readGEBCO.bathy(file.path(datadir,"Mapping","GEBCO","gebco_2023_AtlCan.nc"))
bathy<-NOAAdata
bathy<-GEBCOdata



# MARFISDATA
marfis <- new.env()
get_data(db='marfis',data.dir=datadir,env=marfis)
nafo4VS = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("4VS",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
nafo3N = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("3N",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
nafo3O = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("3O",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
nafo3PS = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("3PS",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
marfisNAFOs = c(nafo3N,nafo3O,nafo3PS,nafo4VS)
marfis$NAFO_UNIT_AREAS = marfis$NAFO_UNIT_AREAS[marfis$NAFO_UNIT_AREAS$NAFO_AREA%in%marfisNAFOs,]
self_filter('marfis',env=marfis)
marfis$NAFO_UNIT_AREAS$NAFO = marfis$NAFO_UNIT_AREAS$NAFO_AREA
colm.names<-c("YEAR","NAFO_UNIT_AREA_ID","RND_WEIGHT_KGS","LATITUDE","LONGITUDE","VR_NUMBER_FISHING","DATE_FISHED","TRIP_ID","SPECIES_CODE","GEAR_CODE")

MarfisData = left_join(marfis$PRO_SPC_INFO[,colm.names],marfis$NAFO_UNIT_AREAS[,c("NAFO","AREA_ID")],by=c("NAFO_UNIT_AREA_ID"="AREA_ID")) %>%
  left_join(.,marfis$VESSELS[,c("LOA","VR_NUMBER")],by=c("VR_NUMBER_FISHING"="VR_NUMBER"))
rm(marfis)


n=6

# site A: shelf edge in 3O near EEZ boundary
pol<-clugenr::points_on_line(c(-51.8, 43.45), c(1, 0.7), seq(0, 0.01188*n, length.out = n))

A<-data.frame(X=pol[,1],Y=pol[,2])

bioMap(xlim=c(-52.3,-51.3),ylim=c(43.2,43.7),isobaths =NULL)
plot(bathy,add =T,deepest.isobath = -1000,shallowest.isobath = -50, step=50,col=rgb(0,0,1,0.1))
plot(bathy,add =T,deepest.isobath = -500,shallowest.isobath = -100, step=400,col='darkblue')
plot(bathy,add =T,deepest.isobath = -200,shallowest.isobath = -200, step=0,col='darkblue',lty=2)
points(Y~X,A,bg='red',pch=21)
with(subset(MarfisData,YEAR>2020 ),points(LONGITUDE, LATITUDE ,pch=16, cex=0.1,col=rgb(0,0,0,0.2)) )



# site B: shelf edge on boundary of 3Ps and 3O
B<-data.frame(X=rep(-54.5,n+1),Y=seq(45.0,by=0.014373,length.out=n+1))

bioMap(xlim=c(-55,-54),ylim=c(44.8,45.3),isobaths =NULL)
plot(bathy,add =T,deepest.isobath = -1000,shallowest.isobath = -50, step=50,col=rgb(0,0,1,0.1))
plot(bathy,add =T,deepest.isobath = -500,shallowest.isobath = -100, step=400,col='darkblue')
plot(bathy,add =T,deepest.isobath = -200,shallowest.isobath = -200, step=0,col='darkblue',lty=2)
plot(st_geometry(NAFO),add=T)
points(Y~X,B,bg='red',pch=21)
with(subset(MarfisData,YEAR>2020 ),points(LONGITUDE, LATITUDE ,pch=16, cex=0.1,col=rgb(0,0,0,0.2)) )



# site C: shelf edge inside Lophelia MPA near the stone fence
pol<-clugenr::points_on_line(c(-57.21, 44.496), c(1, 0), seq(0, 0.0168*n, length.out = n))
pol2<-clugenr::points_on_line(pol[3,], c(0.85, -1), seq(0, 0.01024*n, length.out = n))
#pol3<-clugenr::points_on_line(c(-57.215, 44.498), c(1, -0.57), seq(0, 0.01312*n, length.out = n))

C<-data.frame(X=c(pol[1:2,1],pol2[1:4,1]),Y=c(pol[1:2,2],pol2[1:4,2]))
#C<-data.frame(X=pol3[,1],Y=pol3[,2])


bioMap(xlim=c(-57.4,-56.9),ylim=c(44.3,44.55),isobaths =NULL)
plot(bathy,add =T,deepest.isobath = -1000,shallowest.isobath = -50, step=50,col=rgb(0,0,1,0.1))
plot(bathy,add =T,deepest.isobath = -500,shallowest.isobath = -100, step=400,col='darkblue')
plot(bathy,add =T,deepest.isobath = -200,shallowest.isobath = -200, step=0,col='darkblue',lty=2)
plot(st_geometry(NAFO),add=T)
plot(st_geometry(Lophelia),add=T,border='purple')
plot(st_geometry(EasternCanyons),add=T,border='green')
points(Y~X,C,bg='red',pch=21)
with(subset(MarfisData,YEAR>2020 ),points(LONGITUDE, LATITUDE ,pch=16, cex=0.1,col=rgb(0,0,0,0.2)) )

# check distance
x<-A
#x<-B
#x<-C

d<-c()
for(i in 1:(n-1)){
  d[i]<-distm (c(x$X[i], x$Y[i]), c(x$X[i+1], x$Y[i+1]), fun = distHaversine)
}
d

# final map
bioMap(xlim=c(-58,-50.5),ylim=c(43,46),isobaths =NULL)

plot(st_geometry(NAFO),add=T)
text(c(-50.75,-52,-55.25,-57),c(44.8,44.8,44.5,44),c("3N","3O","3Ps","4Vs"))
points(Y~X,A,bg='red',cex=0.7,pch=21)
points(Y~X,B,bg='red',cex=0.7,pch=21)
points(Y~X,C,bg='red',cex=0.7,pch=21)
#points(Y~X,RC,bg='green',cex=0.5,pch=21)
#points(Y~X,LC,bg='green',cex=0.5,pch=21)
#text(c(NO$X[1],HC$X[1],PO$X[1],RC$X[1],LC$X[1]),c(NO$Y[1],HC$Y[1],PO$Y[1],RC$Y[1],LC$Y[1]),labels=c(8,6,10,5,8),col=c('red','red','green','green','green'),pos=2)

plot(bathy,add =T,deepest.isobath = -500,shallowest.isobath = -100, step=400,col='darkblue')
plot(bathy,add =T,deepest.isobath = -200,shallowest.isobath = -200, step=0,col='darkblue',lty=2)

plot(bathy,add =T,deepest.isobath = -1000,shallowest.isobath = -50, step=50,col=rgb(0,0,1,0.1))

plot(bathy,add =T,deepest.isobath = -7000,shallowest.isobath = -1000, step=1000,col=rgb(0,0,0,0.1))


sets<-read.csv(file.path(datadir,"StratifiedRandom_2024final_St38_May27.csv"))
with(sets,points(lon.DecDeg,lat.DecDeg,pch=21,bg='yellow',cex=0.7))
plot(st_geometry(EasternCanyons),add=T,border='green')

#HC
bioMap(xlim=c(-56,-55.7),ylim=c(44.8,45),isobaths =bls$bl,bathcol = bls$bcol)
points(Y~X,HC,bg='red',pch=21)




#PO
bioMap(xlim=c(-55,-54.5),ylim=c(45,45.3),isobaths =bls$bl,bathcol = bls$bcol)
points(Y~X,PO,bg='red',pch=21)



bioMap(xlim=c(-58,-53),ylim=c(44,46),isobaths =bls$bl,bathcol = bls$bcol)

plot(st_geometry(NAFO),add=T)
points(Y~X,HC,bg='red',cex=0.5,pch=21)
points(Y~X,PO,bg='red',cex=0.5,pch=21)


#NO

n=8

pol<-clugenr::points_on_line(c(-52, 43.48), c(1, 0.75), seq(0, 0.01965*n, length.out = n))

NO<-data.frame(X=pol[,1],Y=pol[,2])

bioMap(xlim=c(-53,-50.8),ylim=c(43,44),isobaths =bls$bl,bathcol = bls$bcol)

plot(st_geometry(NAFO),add=T)
points(Y~X,NO,bg='red',cex=0.5,pch=21)

#LC

n=8

pol<-clugenr::points_on_line(c(-56.65, 45.55), c(1, 1), seq(0, 0.01965*n, length.out = n))

LC<-data.frame(X=pol[,1],Y=pol[,2])

bioMap(xlim=c(-58,-56),ylim=c(45,46),isobaths =bls$bl,bathcol = bls$bcol)

plot(st_geometry(NAFO),add=T)
points(Y~X,LC,bg='green',cex=0.5,pch=21)


#RC

n=5

pol<-clugenr::points_on_line(c(-57.27, 44.5), c(1, 0), seq(0, 0.034*n, length.out = n))

RC<-data.frame(X=pol[,1],Y=pol[,2])
x<-RC

bioMap(xlim=c(-58,-56),ylim=c(44,45),isobaths =bls$bl)
plot(bathy,add =T,deepest.isobath = -500,shallowest.isobath = -100, step=400,col='darkblue')


plot(st_geometry(NAFO),add=T)
points(Y~X,RC,bg='green',cex=0.5,pch=21)



