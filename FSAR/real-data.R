#' Simulate fisheries data to demonstrate FSAR figures

#' @param format Long or wide data frame?
#'
#' @return A data frame
#' @export
#'
#' @examples
#' fsar_data()
fsar_data <- function(Assessment.Year=2024,datadir=datadir,update.data=F) {


  if(update.data){
    Mar.datawrangling::get_data(db='isdb',data.dir=datadir,fn.oracle.username = uid, fn.oracle.password = pwd, fn.oracle.dsn = 'ptran',usepkg='roracle',force.extract=T)
    RVdata<-get4VWXRV(uid, pwd, use.local=F,datadir=datadir)
  }

  categories <- c("Catch", "Biomass", "Fishing", "Recruitment")

  # model output
  load("fsardata/ModelOuputFSAR.rdata")
  load("fsardata/envelope90.rdata")
  yrs<-1970:2050
  projyrs<-2022:2032
  proj<-envelope90[,,,which(yrs%in%projyrs)]


  ### Catch

  ##update from Landings SCript each year

  Catch=read.csv("fsardata/figuredata_markdown_2024.csv")
  Catch=subset(Catch,Year>1969)
  ts1=data.frame(
    panel.category = rep(categories[1],nrow(Catch)),
    year = Catch$Year,
    ts.name = Catch$Source,
    ts.value = Catch$landings_mt)


  ######

  ### SurveyBiomass
  HSRandom_Index<-StratifiedRandomSurveyIndex(datadir=datadir,yrs=2017:Assessment.Year)
  ts2<-rbind(
    data.frame(
      panel.category = rep(categories[2],nrow(HSRandom_Index)),
      year = HSRandom_Index$Year,
      ts.name = rep("HSobs",nrow(HSRandom_Index)),
      ts.value = HSRandom_Index$KgPKH/qRS/1000),
    data.frame(
      panel.category = rep(categories[2],nrow(HSRandom_Index)-2),
      year = HSRandom_Index$Year[-(1:2)],
      ts.name = rep("HSobs_3yrm",nrow(HSRandom_Index)-2),
      ts.value = HSRandom_Index$mean3_biomass[-(1:2)]/qRS/1000),
    data.frame(
      panel.category = rep(categories[2],length(vulnB)),
      year = as.numeric(names(vulnB)),
      ts.name = rep("HSpred",length(vulnB)),
      ts.value = vulnB),
    data.frame(
      panel.category = rep(categories[2],length(vulnB)),
      year = as.numeric(names(vulnB)),
      ts.name = rep("HSpredlow",length(vulnB)),
      ts.value = vulnB_ci[1,]),
    data.frame(
      panel.category = rep(categories[2],length(vulnB)),
      year = as.numeric(names(vulnB)),
      ts.name = rep("HSpredhigh",length(vulnB)),
      ts.value = vulnB_ci[2,]),
    data.frame(
      panel.category = rep(categories[2],length(projyrs)),
      year = projyrs,
      ts.name = rep("HSproj",length(projyrs)),
      ts.value = proj[2,]),
    data.frame(
      panel.category = rep(categories[2],length(projyrs)),
      year = projyrs,
      ts.name = rep("HSprojlow",length(projyrs)),
      ts.value = proj[1,]),
    data.frame(
      panel.category = rep(categories[2],length(projyrs)),
      year = projyrs,
      ts.name = rep("HSprojhigh",length(projyrs)),
      ts.value = proj[3,])
  )



  ### Fishing
  ts3<-rbind(
    data.frame(
      panel.category = rep(categories[3],length(Ut)),
      year = as.numeric(names(Ut)),
      ts.name = rep("Ut",length(Ut)),
      ts.value = Ut),
    data.frame(
      panel.category = rep(categories[3],length(Ut)),
      year = as.numeric(names(Ut)),
      ts.name = rep("Utlow",length(Ut)),
      ts.value = Ut_low),
    data.frame(
      panel.category = rep(categories[3],length(Ut)),
      year = as.numeric(names(Ut)),
      ts.name = rep("Uthigh",length(Ut)),
      ts.value = Ut_high),
    data.frame(
      panel.category = rep(categories[3],length(Mt)),
      year = as.numeric(names(Mt)),
      ts.name = rep("Mt",length(Mt)),
      ts.value = Mt),
    data.frame(
      panel.category = rep(categories[3],length(Mt)),
      year = as.numeric(names(Mt)),
      ts.name = rep("Mtlow",length(Mt)),
      ts.value = Mt_ci[1,]),
    data.frame(
      panel.category = rep(categories[3],length(Mt)),
      year = as.numeric(names(Mt)),
      ts.name = rep("Mthigh",length(Mt)),
      ts.value = Mt_ci[2,])
  )


  ### Recruitment
  RVdata<-get4VWXRV(uid, pwd, use.local=T,datadir=datadir)
  RVinputs<-prepRVdata(RVdata,years=1970:Assessment.Year, raw=T)
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
      ts.value = vulnN),
    data.frame(
      panel.category = rep(categories[4],length(vulnN)),
      year = as.numeric(names(vulnN)),
      ts.name = rep("RVpredlow",length(vulnN)),
      ts.value = vulnN_ci[1,]),
    data.frame(
      panel.category = rep(categories[4],length(vulnN)),
      year = as.numeric(names(vulnN)),
      ts.name = rep("RVpredhigh",length(vulnN)),
      ts.value = vulnN_ci[2,])
  )




  return(df=rbind(ts1,ts2,ts3,ts4))

}
