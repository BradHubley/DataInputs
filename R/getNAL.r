#' @export
getNAL<-function(LF, sex='all'){

  if(sex=='all'){
    NAL<-do.call("rbind",lapply(LF,colSums))

  }else{
    sx=c(1,2,0)
    NAL<-do.call("rbind",lapply(LF,subset,sex==sx))

  }
  row.names(NAL)<-names(LF)
  return(NAL)
}
