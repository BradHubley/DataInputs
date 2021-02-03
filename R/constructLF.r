constructLF<-function(bins,Yrs,sx=c(1,2,0)){

  LF=list()
  bins = paste0("L",bins[-1])
  FixedSurvey$N_MEASURED <- rowSums(FixedSurvey[,bins])

  for(i in 1:length(Yrs)){

     LF[[i]]=t(sapply(sx,function(s){colSums(subset(FixedSurvey,YEAR==Yrs[i]&SEXCD_ID==s,bins),na.rm=T)}))

  }
  return(LF)
}
