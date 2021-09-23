#' @export
getPAL<-function(LF, sex){

  if(sex=='all'){
    PAL<-proportions(do.call("rbind",lapply(LF,colSums)),1)

  }else{
    sx=c(1,2,0)
    PAL<-proportions(do.call("rbind",lapply(LF,subset,sex==sx)),1)

  }
  return(PAL)
}
