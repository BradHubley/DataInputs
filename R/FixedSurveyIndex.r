#' @export
FixedSurveyIndex<-function(datadir,yrs,restrict100=T,old.model=F,use.calc.wt=F){

  FSindexData <- FixedSurveyData(datadir=datadir, by.sex=F,adj.calc.wt=use.calc.wt,add.LF=use.calc.wt)

  if(restrict100){
    load(file.path(datadir,"Survey","HSFixed100Stations.rdata"))
    FSindexData<- subset(FSindexData,STATION%in%stations100)
  }
  FSindexData<- subset(FSindexData,YEAR%in%yrs)
#browser()
  FSindexData$EST_NUM_CAUGHT[is.na(FSindexData$EST_NUM_CAUGHT)]<-0
  FSindexData$EST_COMBINED_WT[is.na(FSindexData$EST_COMBINED_WT)]<-0
  FSindexData$NUM_HOOK_HAUL[is.na(FSindexData$NUM_HOOK_HAUL)]<-1000

  FSindexData$NPKH <-  FSindexData$EST_NUM_CAUGHT/(FSindexData$NUM_HOOK_HAUL/1000)
  FSindexData$WPKH <-  FSindexData$EST_COMBINED_WT/(FSindexData$NUM_HOOK_HAUL/1000)
  if(use.calc.wt)FSindexData$WPKH <-  FSindexData$calc_weight/(FSindexData$NUM_HOOK_HAUL/1000)

  Year<-sort(unique(FSindexData$YEAR))
  KgPKH<-with(FSindexData,tapply(WPKH,YEAR,mean))
  KgPKHse<-with(FSindexData,tapply(WPKH,YEAR,sd))
  NPKH<-with(FSindexData,tapply(NPKH,YEAR,mean))
  NPKHse<-with(FSindexData,tapply(NPKH,YEAR,sd))

  out<-data.frame(Year=Year,KgPKH=KgPKH,KgPKHse=KgPKHse,NPKH=NPKH,NPKHse=NPKHse)
  out<-subset(out,Year%in%yrs)

  if(old.model){

    out<-makeOldFSIndex(FSindexData,do.plot=F)$index
  }

  mean3_biomass <- NULL
  for(i in 3:nrow(out)){
    hold_mean <- mean(out[i:(i-2),2])
    mean3_biomass <- c(mean3_biomass, hold_mean)
  }

  out$mean3_biomass <- c(rep(NA, 2), mean3_biomass)


  return(out)

}
