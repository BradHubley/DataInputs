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

  # model output
  load("ModelOuputFSAR.rdata")

  categories <- c("Catch", "SurveyBiomass", "Fishing", "Recruitment")


  ### Catch

  # annual TAC table
  ##update TAC each year
  TAC = data.frame(Year=1988:Assessment.Year,TAC=c(3200,3200,3200,3200,3200,3200,1500,850,850,850,850,850,1000,1150,1150,1300,1300,1375,1475,1475,1475,1700,1850,1850,2128,2447,2563,2738,3149,3621,4164,4789,5507,5445,4807, 4744, 4927))
  ### add landings script here



  #######

  ### SurveyBiomass

  # stratified random index as designed
  HSRandom_Index<-StratifiedRandomSurveyIndex(datadir=datadir,yrs=2017:Assessment.Year)

  ### Fishing
  Fishing=data.frame(Year=as.numeric(names(U_T)),U=U_t)


  ### Recruitment
  RVdata<-get4VWXRV(uid, pwd, use.local=T,datadir=datadir)
  RVinputs<-prepRVdata(RVdata,bins=bins,years=1970:Current.Year, raw=T)
  NSRV_Index<-RVinputs$Index
  names(NSRV_Index)[1]<-"Year"



  return(df=list(Catch=TAC,SurveyBiomass=HSRandom_Index,Fishing=Fishing,Recruitment=NSRV_Index))

}
