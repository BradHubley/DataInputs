FixedSurveyIndex<-function(datadir,yrs){

  FSindexData <- FixedSurveyData(datadir=datadir, add.LF=F,by.sex=F)
  FSindexData$EST_NUM_CAUGHT[is.na(FSindexData$EST_NUM_CAUGHT)]<-0
  FSindexData$EST_COMBINED_WT[is.na(FSindexData$EST_COMBINED_WT)]<-0
  FSindexData$NUM_HOOK_HAUL[is.na(FSindexData$NUM_HOOK_HAUL)]<-1000
  load(file.path(datadir,"Survey","HSFixed100Stations.rdata"))

  FSindexData$NPKH <-  FSindexData$EST_NUM_CAUGHT/(FSindexData$NUM_HOOK_HAUL/1000)
  FSindexData$WPKH <-  FSindexData$EST_COMBINED_WT/(FSindexData$NUM_HOOK_HAUL/1000)

  Year<-sort(unique(FSindexData$YEAR))
  NPKH<-with(subset(FSindexData,STATION%in%stations100),tapply(NPKH,YEAR,mean))
  NPKHse<-with(subset(FSindexData,STATION%in%stations100),tapply(NPKH,YEAR,sd))
  KgPKH<-with(subset(FSindexData,STATION%in%stations100),tapply(WPKH,YEAR,mean))
  KgPKHse<-with(subset(FSindexData,STATION%in%stations100),tapply(WPKH,YEAR,sd))

  out<-data.frame(Year=Year,NPKH=NPKH,NPKHse=NPKHse,KgPKH=KgPKH,KgPKHse=KgPKHse)

  return(subset(out,Year%in%yrs))

}
