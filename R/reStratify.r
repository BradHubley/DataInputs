#' @export
reStratify <- function(sets,strata,lines=F){


  sets$EID<-sets$FISHSET_ID

  if(lines==T){
    sets$Y<-with(sets,apply(cbind(LAT1,LAT2),1,mean))
    sets$X<-with(sets,apply(cbind(LONG1,LONG2),1,mean))
  } else{
    sets$Y<-sets$LAT1
    sets$X<-sets$LONG1

  }

  key<-findPolys(sets,strata)
  sets<-merge(sets,key,all=T)

  return(sets)
}
