#' @export
FixedSurveyIndex<-function(datadir,yrs,restrict100=T,old.model=F,use.calc.wt=F,nafo){

  FSindexData <- FixedSurveyData(datadir=datadir, by.sex=F,adj.calc.wt=use.calc.wt,add.LF=use.calc.wt)

  if(restrict100){
    load(file.path(datadir,"Survey","HSFixed100Stations.rdata"))
    FSindexData<- subset(FSindexData,STATION%in%stations100)
  }
  if(missing(nafo))nafo<-unique(FSindexData$NAFAREA_ID)
  FSindexData<- subset(FSindexData,YEAR%in%yrs&NAFAREA_ID%in%nafo)
#browser()
  FSindexData$EST_NUM_CAUGHT[is.na(FSindexData$EST_NUM_CAUGHT)]<-0
  FSindexData$EST_COMBINED_WT[is.na(FSindexData$EST_COMBINED_WT)]<-0
  FSindexData$NUM_HOOK_HAUL[is.na(FSindexData$NUM_HOOK_HAUL)]<-1000

  FSindexData$NPKH <-  FSindexData$EST_NUM_CAUGHT/(FSindexData$NUM_HOOK_HAUL/1000)
  FSindexData$WPKH <-  FSindexData$EST_COMBINED_WT/(FSindexData$NUM_HOOK_HAUL/1000)
  if(use.calc.wt)FSindexData$WPKH <-  FSindexData$calc_weight/(FSindexData$NUM_HOOK_HAUL/1000)

  Year<-sort(unique(FSindexData$YEAR))
  n<-with(FSindexData,tapply(WPKH,YEAR,length))
  KgPKH<-with(FSindexData,tapply(WPKH,YEAR,mean))
  KgPKHse<-with(FSindexData,tapply(WPKH,YEAR,sd))
  NPKH<-with(FSindexData,tapply(NPKH,YEAR,mean))
  NPKHse<-with(FSindexData,tapply(NPKH,YEAR,sd))

  out<-data.frame(Year=Year,n=n,KgPKH=KgPKH,KgPKHse=KgPKHse,NPKH=NPKH,NPKHse=NPKHse)
  out<-subset(out,Year%in%yrs)

  if(old.model){

    out<-makeOldFSIndex(FSindexData,do.plot=F)$index
  }
#browser()
  out$mean3_biomass <- mavg(out$KgPKH)


  return(out)

}
