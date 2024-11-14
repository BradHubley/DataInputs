#' Simulate fisheries data to demonstrate FSAR figures

#' @param format Long or wide data frame?
#'
#' @return A data frame
#' @export
#'
#' @examples
#' fsar_data()
fsar_data <- function(Assessment.Year=2024,datadir,update.data=F) {


  if(update.data){
    Mar.datawrangling::get_data(db='isdb',data.dir=datadir,fn.oracle.username = uid, fn.oracle.password = pwd, fn.oracle.dsn = 'ptran',usepkg='roracle',force.extract=T)
    RVdata<-get4VWXRV(uid, pwd, use.local=F,datadir=datadir)
  }

  categories <- c("Catch", "Biomass", "Fishing", "Recruitment")

  # model output
  load("ModelOuputFSAR.rdata")

  ### Catch

  # annual TAC table
  ##update TAC each year
  ### add landings script here
  Catch=NULL

  TAC = data.frame(Year=1988:Assessment.Year,TAC=c(3200,3200,3200,3200,3200,3200,1500,850,850,850,850,850,1000,1150,1150,1300,1300,1375,1475,1475,1475,1700,1850,1850,2128,2447,2563,2738,3149,3621,4164,4789,5507,5445,4807, 4744, 4927))


  ######

  ### SurveyBiomass
  HSRandom_Index<-StratifiedRandomSurveyIndex(datadir=datadir,yrs=2017:Assessment.Year)
  ts2<-rbind(
    data.frame(
      panel.category = rep(categories[2],nrow(HSRandom_Index)),
      year = HSRandom_Index$Year,
      ts.name = rep("HSobs",nrow(NSRV_Index)),
      ts.value = HSRandom_Index$KgPKH/qRS/1000),
    data.frame(
      panel.category = rep(categories[2],nrow(HSRandom_Index)-2),
      year = HSRandom_Index$Year[-(1:2)],
      ts.name = rep("HSobs",nrow(NSRV_Index)-2),
      ts.value = HSRandom_Index$KgPKH[-(1:2)]/qRS/1000),
    data.frame(
      panel.category = rep(categories[2],length(vulnB)),
      year = as.numeric(names(vulnB)),
      ts.name = rep("HSpred",length(vulnB)),
      ts.value = vulnB)
  )



  ### Fishing
  ts3<-data.frame(
      panel.category = rep(categories[3],nrow(U_t)),
      year = as.numeric(names(U_T)),
      ts.name = rep("Ut",nrow(U_T)),
      ts.value = U_t)


  ### Recruitment
  RVdata<-get4VWXRV(uid, pwd, use.local=T,datadir=datadir)
  RVinputs<-prepRVdata(RVdata,bins=bins,years=1970:Assessment.Year, raw=T)
  NSRV_Index<-RVinputs$Index
  ts4<-rbind(
    data.frame(
      panel.category = rep(categories[4],nrow(NSRV_Index)),
      year = NSRV_Index$year,
      ts.name = rep("RVobs",nrow(NSRV_Index)),
      ts.value = NSRV_Index$NPT/qRV),
    data.frame(
      panel.category = rep(categories[4],length(vulnN)),
      year = as.numeric(names(vulnN)),
      ts.name = rep("RVpred",length(vulnN)),
      ts.value = vulnN)
    )




  return(df=rbind(ts1,ts2,ts3,ts4))

}
