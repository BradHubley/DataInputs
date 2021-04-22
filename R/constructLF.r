#' @export
constructLF<-function(LFdata, bins,Yrs,sx=c(1,2,0)){

  LF=list()
  bins = paste0("L",bins[-1])
  LFdata$N_MEASURED <- rowSums(LFdata[,bins])

  for(i in 1:length(Yrs)){

     LF[[i]]=t(sapply(sx,function(s){colSums(subset(LFdata,YEAR==Yrs[i]&SEXCD_ID==s,bins),na.rm=T)}))

  }
  names(LF) = Yrs

  return(LF)
}
