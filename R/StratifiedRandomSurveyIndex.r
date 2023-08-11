#' @export
StratifiedRandomSurveyIndex<-function(datadir,yrs,output='stratified.mean',nadj=1,use.calc.weight=F,restratify=F, France=F){

  RSindexData <- RandomSurveyData(datadir=datadir, add.LF=T,by.sex=F, LF.from = "ISFISH")
  RSindexData$STRAT <- as.numeric(substr(RSindexData$ASSIGNED_STRATUM_ID,2,3))

  load(file.path(datadir,"Survey","SurveyStrata.rdata")) # check this to make sure it's up to date
  areas<- StrataAreas$area
  strata<-StrataAreas$PID
  polys<-surveyStrataPolyLL

  load(file.path(datadir,"Survey","SurveyStrata2022.rdata")) # check this to make sure it's up to date
  areas2<- StrataAreas$area
  strata2<-StrataAreas$PID
  polys2<-surveyStrataPolyLL

  if(France){
    load(file.path(datadir,"Survey","SurveyStrata.rdata")) # check this to make sure it's up to date
    RSindexData<-reStratify(RSindexData,subset(surveyStrataPolyLL,PID%in%c(33,41:43)))
    RSindexData$STRAT<-RSindexData$PID
    areas2<-read.csv(file.path(datadir,"Survey","SPMareas.csv"))$area
  }


  RSindexData<- subset(RSindexData,YEAR%in%yrs)

  if (restratify){
    RSindexData<-reStratify(RSindexData,polys2)
    RSindexData$STRAT<-RSindexData$PID
  }


  RSindexData$EST_NUM_CAUGHT[is.na(RSindexData$EST_NUM_CAUGHT)]<-0
  RSindexData$EST_COMBINED_WT[is.na(RSindexData$EST_COMBINED_WT)]<-0
  RSindexData$NUM_HOOK_HAUL[is.na(RSindexData$NUM_HOOK_HAUL)]<-1000

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

    # Stratified mean and variance
    n<-with(subset(RSindexData,YEAR==yrs[i]),tapply(STATION,STRAT,length))*nadj
    Nh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(NPKH,STRAT,mean))
    Nt<-with(subset(RSindexData,YEAR==yrs[i]),tapply(NPKH,STRAT,sum))
    NVh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(NPKH,STRAT,var))
    Bh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(WPKH,STRAT,mean))
    BVh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(WPKH,STRAT,var))
    sets[i]<-sum(n)
    N[i]<-sum(Nh*areas)
    Nse[i]<-sqrt(sum(NVh/n*areas^2))
    Nti[[i]]<-Nt
    B[i]<-sum(Bh*areas)
    Bse[i]<-sqrt(sum(BVh/n*areas^2))
    print(paste(yrs[i],round(N[i]/sum(areas),2),round(B[i]/sum(areas))))
  }
#browser()
  save(Nti,file="Nti.rdata")

  sN<-with(RSindexData,tapply(STATION,YEAR,length))
  sKgPKH<-with(RSindexData,tapply(WPKH,YEAR,mean))
  sKgPKHse<-with(RSindexData,tapply(WPKH,YEAR,sd))/sqrt(sN)
  sNPKH<-with(RSindexData,tapply(NPKH,YEAR,mean))
  sNPKHse<-with(RSindexData,tapply(NPKH,YEAR,sd))/sqrt(sN)

  if(output=='stratified.estimate')out<-data.frame(Year=yrs,n=sets,B=B,Bse=Bse,N=N,Nse=Nse)
  if(output=='stratified.mean')out<-data.frame(Year=yrs,n=sets,KgPKH=B/sum(areas),KgPKHse=Bse/sum(areas),NPKH=N/sum(areas),NPKHse=Nse/sum(areas))
  if(output=='simple.mean')out<-data.frame(Year=yrs,n=sets,KgPKH=sKgPKH,KgPKHse=sKgPKHse,NPKH=sNPKH,NPKHse=sNPKHse)


  out$mean3_biomass <- mavg(out$KgPKH)


  return(out)

}
