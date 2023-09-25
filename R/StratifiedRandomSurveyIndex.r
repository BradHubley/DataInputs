#' @export
StratifiedRandomSurveyIndex<-function(datadir,yrs,output='stratified.mean',sp=30,nadj=1,use.calc.weight=F,restratify=F, France=F, select.strata){

  RSindexData <- RandomSurveyData(sp=sp,datadir=datadir, add.LF=T,by.sex=F, LF.from = "ISFISH")
  RSindexData$STRAT <- as.numeric(substr(RSindexData$ASSIGNED_STRATUM_ID,2,3)) # make strata.id numeric
  if(!missing(select.strata))RSindexData<-subset(RSindexData,STRAT%in%select.strata)

  load(file.path(datadir,"Survey","SurveyStrata.rdata")) # These are the original strata (2017-2021)
  #if(!missing(select.strata))StrataAreas<-subset(StrataAreas,PID%in%select.strata)
  areas<- data.frame(StrataAreas)

  load(file.path(datadir,"Survey","SurveyStrata2022.rdata")) # these strata were introduced in 2022
 # if(!missing(select.strata))StrataAreas<-subset(StrataAreas,PID%in%select.strata)
  areas2<- data.frame(StrataAreas)

  # bit to deal with missing sets in 52
  if(1){
    areas$area[areas$PID==52]<-areas$area[areas$PID==52]+areas$area[areas$PID==53]
    areas<-areas[-nrow(areas),]
    areas2$area[areas2$PID==52]<-areas2$area[areas2$PID==52]+areas2$area[areas2$PID==53]
    areas2<-areas2[-nrow(areas2),]
    RSindexData$STRAT[RSindexData$STRAT==53] <- 52

  }

  # to calculate index for St. Pierre and Michelon EEZ (keyhole, baguette)
  if(France){
    load(file.path(datadir,"Survey","SurveyStrata.rdata"))
    RSindexData<-reStratify(RSindexData,subset(surveyStrataPolyLL,PID%in%c(33,41:43)))
    RSindexData$STRAT<-RSindexData$PID
    areas2<-read.csv(file.path(datadir,"Survey","SPMareas.csv"))
  }


  RSindexData<- subset(RSindexData,YEAR%in%yrs)

  # restratify index to use new strata for whole timeseries
  if (restratify){
    RSindexData<-reStratify(RSindexData,polys2)
    RSindexData$STRAT<-RSindexData$PID
  }

  # address NAs
  RSindexData$EST_NUM_CAUGHT[is.na(RSindexData$EST_NUM_CAUGHT)]<-0
  RSindexData$EST_COMBINED_WT[is.na(RSindexData$EST_COMBINED_WT)]<-0
  RSindexData$NUM_HOOK_HAUL[is.na(RSindexData$NUM_HOOK_HAUL)]<-1000

  # standardize for 1000 hooks
  RSindexData$NPKH <-  RSindexData$EST_NUM_CAUGHT/(RSindexData$NUM_HOOK_HAUL/1000)
  RSindexData$WPKH <-  RSindexData$EST_COMBINED_WT/(RSindexData$NUM_HOOK_HAUL/1000)
  if(use.calc.weight)  RSindexData$WPKH <-  RSindexData$calc_weight/(RSindexData$NUM_HOOK_HAUL/1000)

  sets<-c()
  N<-c()
  B<-c()
  Nse<-c()
  Bse<-c()
  Nti<-list()


  print(paste("Year", "N/KH", "Kg/KH"))
  for (i in 1:length(yrs)){

    if(yrs[i]>2021 || restratify || France )areas<-areas2 # new strata areas in 2022
    total.area<-sum(areas$area)


    # Stratified mean and variance
    n<-with(subset(RSindexData,YEAR==yrs[i]),tapply(STATION,STRAT,length))*nadj
    Nh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(NPKH,STRAT,mean))
    Nt<-with(subset(RSindexData,YEAR==yrs[i]),tapply(NPKH,STRAT,sum))
    NVh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(NPKH,STRAT,var))
    Bh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(WPKH,STRAT,mean))
    BVh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(WPKH,STRAT,var))

    Nti[[i]]<-merge(areas,data.frame(PID=as.numeric(names(n)),n,Nh,Nt,NVh,Bh,BVh),all=T)
    sets[i]<-sum(n)
    N[i]<-with(Nti[[i]],sum(Nh*area))
    Nse[i]<-with(Nti[[i]],sqrt(sum(NVh/n*area^2)))
    B[i]<-with(Nti[[i]],sum(Bh*area))
    Bse[i]<-with(Nti[[i]],sqrt(sum(BVh/n*area^2)))
    print(paste(yrs[i],round(N[i]/sum(areas$area),2),round(B[i]/sum(areas$area))))
  }
#browser()
  save(Nti,file="Nti.rdata")

  sN<-with(RSindexData,tapply(STATION,YEAR,length))
  sKgPKH<-with(RSindexData,tapply(WPKH,YEAR,mean))
  sKgPKHse<-with(RSindexData,tapply(WPKH,YEAR,sd))/sqrt(sN)
  sNPKH<-with(RSindexData,tapply(NPKH,YEAR,mean))
  sNPKHse<-with(RSindexData,tapply(NPKH,YEAR,sd))/sqrt(sN)

  if(output=='stratified.estimate')out<-data.frame(Year=yrs,n=sets,B=B,Bse=Bse,N=N,Nse=Nse)
  if(output=='stratified.mean')out<-data.frame(Year=yrs,n=sets,KgPKH=B/sum(areas$area),KgPKHse=Bse/sum(areas$area),NPKH=N/sum(areas$area),NPKHse=Nse/sum(areas$area))
  if(output=='simple.mean')out<-data.frame(Year=yrs,n=sets,KgPKH=sKgPKH,KgPKHse=sKgPKHse,NPKH=sNPKH,NPKHse=sNPKHse)


  if(output=='stratified.estimate'){
    out$mean3_biomass <- mavg(out$B)
  }else {out$mean3_biomass <- mavg(out$KgPKH)}

  return(out)

}
