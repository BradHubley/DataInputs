#' @export
StratifiedRandomSurveyIndex<-function(datadir,yrs,output='stratified.mean',sp=30,nadj=1,use.calc.weight=F,restratify=F, AltArea='n', select.strata, ZeroStrata=T, combine.strata=NULL){

  RSindexData <- RandomSurveyData(sp=sp,datadir=datadir, add.LF=T,by.sex=F, LF.from = "ISFISH")
  RSindexData$STRAT <- as.numeric(substr(RSindexData$ASSIGNED_STRATUM_ID,2,3)) # make strata.id numeric
  if(!missing(select.strata))RSindexData<-subset(RSindexData,STRAT%in%select.strata)

  load(file.path(datadir,"Survey","SurveyStrata.rdata")) # These are the original strata (2017-2021)
  if(!missing(select.strata))StrataAreas<-subset(StrataAreas,PID%in%select.strata)
  areas<- data.frame(StrataAreas)

  load(file.path(datadir,"Survey","SurveyStrata2022.rdata")) # these strata were introduced in 2022
   if(!missing(select.strata))StrataAreas<-subset(StrataAreas,PID%in%select.strata)
  areas2<- data.frame(StrataAreas)

  load(file.path(datadir,"Survey","SurveyStrata2023.rdata")) # these strata were introduced in 2023
  if(!missing(select.strata))StrataAreas<-subset(StrataAreas,PID%in%select.strata)
  areas3<- data.frame(StrataAreas)

  load(file.path(datadir,"Survey","SurveyStrata2024.rdata")) # these strata were introduced in 2024
  if(!missing(select.strata))StrataAreas<-subset(StrataAreas,PID%in%select.strata)
  areas4<- data.frame(StrataAreas)

  if(!ZeroStrata){
    load(file.path(datadir,"Survey","SurveyStrata2023NoBO.rdata")) # these strata were introduced in 2023
     if(!missing(select.strata))StrataAreas<-subset(StrataAreas,PID%in%select.strata)
    areas3<- data.frame(StrataAreas)
  }


  # to calculate index for St. Pierre and Michelon EEZ (keyhole, baguette)
  if(AltArea=='France'){
    load(file.path(datadir,"Survey","SurveyStrata.rdata"))
    RSindexData<-reStratify(RSindexData,subset(surveyStrataPolyLL,PID%in%c(33,41:43)))
    RSindexData$STRAT<-RSindexData$PID
    areas<-areas2<-areas3<-read.csv(file.path(datadir,"Survey","SPMareas.csv"))
  }

  # to calculate index for St. Pierre and Michelon EEZ (keyhole, baguette)
  if(AltArea=='NRA'){
    load(file.path(datadir,"Survey","SurveyStrata.rdata"))
    RSindexData<-reStratify(RSindexData,subset(surveyStrataPolyLL,PID%in%c(51:53)))
    RSindexData$STRAT<-RSindexData$PID
    areas<-areas2<-areas3<-read.csv(file.path(datadir,"Survey","NRAareas.csv"))
  }


  # to calculate index for 3Ps
  if(AltArea=='3Ps'){
    load(file.path(datadir,"Survey","SurveyStrata.rdata"))
    RSindexData<-reStratify(RSindexData,subset(surveyStrataPolyLL,PID%in%c(41:43)))
    RSindexData$STRAT<-RSindexData$PID
    RSindexData<-subset(RSindexData,NAFAREA_ID=='3Ps')
    areas<-areas2<-areas3<-read.csv(file.path(datadir,"Survey","NAFO3PSareas.csv"))
  }

  if(missing(select.strata))select.strata<-unique(RSindexData$STRAT)

  RSindexData<- subset(RSindexData,YEAR%in%yrs&STRAT%in%select.strata)
#browser()
  # restratify index to use new strata for whole timeseries
  if (restratify){
    #load(file.path(datadir,"Survey","SurveyStrataBOpre2022.rdata")) # these strata were introduced in 2023
    # if(!missing(select.strata))StrataAreas<-subset(StrataAreas,PID%in%select.strata)
    #areas3<- data.frame(StrataAreas)
    if(ZeroStrata){
      load(file.path(datadir,"Survey","SurveyStrataBOpre2022.rdata")) # these strata were introduced in 2023
      # if(!missing(select.strata))StrataAreas<-subset(StrataAreas,PID%in%select.strata)
      areas3<- data.frame(StrataAreas)
    }

    RSindexData<-reStratify(RSindexData,surveyStrataPolyLL,lines=T)
    RSindexData$STRAT<-RSindexData$PID
    areas<-areas2<-areas3 # make all the same
  }

  # wrote this bit to deal with missing sets in 52 (in 2023 )
  # combine.strata =c(52,53), can use for combining other strata too
  if(!is.null(combine.strata)){

    areas$area[areas$PID==combine.strata[1]]<-sum(areas$area[areas$PID%in%combine.strata])
    areas<-areas[!areas$PID%in%combine.strata[-1],]
    areas2$area[areas2$PID==combine.strata[1]]<-sum(areas2$area[areas2$PID%in%combine.strata])
    areas2<-areas2[!areas2$PID%in%combine.strata[-1],]
    areas3$area[areas3$PID==combine.strata[1]]<-sum(areas3$area[areas3$PID%in%combine.strata])
    areas3<-areas3[!areas3$PID%in%combine.strata[-1],]
    areas4$area[areas4$PID==combine.strata[1]]<-sum(areas4$area[areas4$PID%in%combine.strata])
    areas4<-areas4[!areas4$PID%in%combine.strata[-1],]
    RSindexData$STRAT[RSindexData$STRAT%in%combine.strata] <- combine.strata[1]

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

    if(yrs[i]==2022 )areas<-areas2 # new strata areas in 2022
    if(yrs[i]==2023 ){
      areas<-areas3 # new strata areas in 2022
      if(ZeroStrata){
        RSindexData$STRAT[RSindexData$YEAR==2023&RSindexData$STRAT%in%c(45,54)] <- 60
      }
      if(!ZeroStrata&&!restratify){
        RSindexData<-reStratify(RSindexData,surveyStrataPolyLL,lines=T)
        RSindexData$STRAT[RSindexData$YEAR==2023&RSindexData$STRAT%in%c(45,54)]<-RSindexData$PID[RSindexData$YEAR==2023&RSindexData$STRAT%in%c(45,54)]
      }
      RSindexData$STRAT[RSindexData$YEAR==2023&RSindexData$STRAT==53] <- 52
      #browser()
    }
    if(yrs[i]>2023 ){
      areas<-areas4 # new strata areas in 2022
    }

    total.area<-sum(areas$area)
#browser()

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
