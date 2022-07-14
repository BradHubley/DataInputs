#' @export
assignStrata<-function(events,polys,coords=c("X","Y")){

  library(PBSmapping)

  if(missing(polys)){
    load(file.path(datadir,"Survey","SurveyStrata2022.rdata"))
    polys<-surveyStrataPolyLL
  }

  events$EID<-1:nrow(events)
  events$X <- events[,coords[1]]
  events$Y <- events[,coords[2]]

  key<-findPolys(events,polys)
  events<-merge(events,key[c('EID','PID')],all=T)
  return(events)

}
