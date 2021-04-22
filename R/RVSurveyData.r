#' @export

RVSurveyData <- function(uid, pwd, bins=seq(5,260,5), by.sex=T,use.local=T,datadir="C:/Users/hubleyb/Documents/Halibut/data"){


  RVdata<-get4VWXRV(uid, pwd, use.local=use.local,datadir=datadir)

  allfish<-RVdata[[2]]
  allfish$MISSION_SET<-paste0(allfish$MISSION,allfish$SETNO)

  fishlengths<-RVdata[[1]]
  fishlengths$MISSION_SET<-paste0(fishlengths$MISSION,fishlengths$SETNO)

  sid=unique(fishlengths$MISSION_SET)
  LF <-list()
  LFnosex<-data.frame('MISSION_SET'=sid,t(sapply(sid,function(s){with(subset(fishlengths,MISSION_SET==s),binNumAtLen(CLEN,FLEN,bins))})))
  #names(LFnosex)[-1]<-paste0("L",bins[-1])
  LFnosex$NUM_MEASURED <- rowSums(LFnosex[,-1],na.rm=T)
  if(by.sex==T){
    sx<-c(1,2,0)
    for(i in 1:3){
      LF[[i]]<-data.frame('MISSION_SET'=sid,'SEXCD_ID'=sx[i],t(sapply(sid,function(s){with(subset(fishlengths,MISSION_SET==s&FSEX==sx[i]), binNumAtLen(CLEN,FLEN,bins))})))
    }
    LF <- do.call("rbind",LF)
    #names(LF)[-(1:2)]<-paste0("L",bins[-1])

    LF <-merge(LF,LFnosex[,c('MISSION_SET','NUM_MEASURED')])
  }
  if(by.sex==F)LF <- LFnosex

  RVSURVEY<- left_join(allfish,LF)
  RVSURVEY$YEAR <- lubridate::year(RVSURVEY$SDATE)

  write.csv(RVSURVEY,file.path(datadir,"RVSurveyHalibutData.csv"),row.names = F)
  return(RVSURVEY)

}
