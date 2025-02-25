#' @export
kdmap<-function(data,xl=c(-74,-47),yl=c(40,52),...){

  require(MASS)
  require(sf)

  rec_den<-st_as_sf(data,coords=2:3,crs=st_crs(4326))
  coords <- st_coordinates(rec_den)
  kde <- kde2d(coords[,1], coords[,2], h=0.8,n = 1000,lims=c(xl,yl))
  bioMap(xlim=xl,ylim=yl,nafo=T,image.lst=kde,color.fun=colorRampPalette(c('white',terrain.colors(30))),...)

}
