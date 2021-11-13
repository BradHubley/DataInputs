StratifiedRandomSurveyIndex<-function(datadir,yrs,output='stratified.mean'){

  RSindexData <- RandomSurveyData(datadir=datadir, add.LF=F,by.sex=F)
  RSindexData$STRAT <- as.numeric(substr(RSindexData$ASSIGNED_STRATUM_ID,2,3))

    load(file.path(datadir,"Survey","SurveyStrata.rdata"))
    areas<- StrataAreas$area
    strata<-StrataAreas$PID


  RSindexData<- subset(RSindexData,YEAR%in%yrs)


  RSindexData$EST_NUM_CAUGHT[is.na(RSindexData$EST_NUM_CAUGHT)]<-0
  RSindexData$EST_COMBINED_WT[is.na(RSindexData$EST_COMBINED_WT)]<-0
  RSindexData$NUM_HOOK_HAUL[is.na(RSindexData$NUM_HOOK_HAUL)]<-1000

  RSindexData$NPKH <-  RSindexData$EST_NUM_CAUGHT/(RSindexData$NUM_HOOK_HAUL/1000)
  RSindexData$WPKH <-  RSindexData$EST_COMBINED_WT/(RSindexData$NUM_HOOK_HAUL/1000)

  N<-c()
  B<-c()
  Nse<-c()
  Bse<-c()


  print(paste("Year", "N/KH", "Kg/KH"))
  for (i in 1:length(yrs)){

    # Stratified mean and variance
    n<-with(subset(RSindexData,YEAR==yrs[i]),tapply(STATION,STRAT,length))
    Nh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(NPKH,STRAT,mean))
    NVh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(NPKH,STRAT,var))
    Bh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(WPKH,STRAT,mean))
    BVh<-with(subset(RSindexData,YEAR==yrs[i]),tapply(WPKH,STRAT,var))
    N[i]<-sum(Nh*areas)
    Nse[i]<-sqrt(sum(NVh/n*areas^2))
    B[i]<-sum(Bh*areas)
    Bse[i]<-sqrt(sum(BVh/n*areas^2))
    print(paste(yrs[i],round(N[i]/sum(areas),2),round(B[i]/sum(areas))))
  }

  sN<-with(RSindexData,tapply(STATION,YEAR,length))
  sKgPKH<-with(RSindexData,tapply(WPKH,YEAR,mean))
  sKgPKHse<-with(RSindexData,tapply(WPKH,YEAR,sd))/sqrt(sN)
  sNPKH<-with(RSindexData,tapply(NPKH,YEAR,mean))
  sNPKHse<-with(RSindexData,tapply(NPKH,YEAR,sd))/sqrt(sN)

  if(output=='stratified.estimate')out<-data.frame(Year=yrs,B=B,Bse=Bse,N=N,Nse=Nse)
  if(output=='stratified.mean')out<-data.frame(Year=yrs,KgPKH=B/sum(areas),KgPKHse=Bse/sum(areas),NPKH=N/sum(areas),NPKHse=Nse/sum(areas))
  if(output=='simple.mean')out<-data.frame(Year=yrs,KgPKH=sKgPKH,KgPKHse=sKgPKHse,NPKH=sNPKH,NPKHse=sNPKHse)


  mean3_biomass <- NULL
  for(i in 3:nrow(out)){
    hold_mean <- mean(out[i:(i-2),2])
    mean3_biomass <- c(mean3_biomass, hold_mean)
  }

  out$mean3_biomass <- c(rep(NA, 2), mean3_biomass)


  return(out)

}
