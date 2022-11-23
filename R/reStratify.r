#' @export
reStratify <- function(sets,strata,lines=F){


  if(lines==T){
    events$X<-with(events,apply(cbind(X1,X2),1,mean))
    events$Y<-with(events,apply(cbind(Y1,Y2),1,mean))
  }

  key<-findPolys(events,polys)
  events<-merge(events,key[c('EID','SID')],all=T)

  return(events)
}
