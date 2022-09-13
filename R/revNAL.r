#' @export
revNAL<-function(NAL){

  LF<-list()
  for(i in 1:nrow(NAL[[1]])){
    y<-matrix(NA,length(NAL),ncol(NAL[[1]]))
    for(j in 1:length(NAL)){
      y[j,]<-unlist(NAL[[j]][i,])
    }
    #names(y)<-names(NAL[[1]])
    LF[[i]]<-y
  }


  names(LF)<-row.names(NAL[[1]])
  return(LF)
}
