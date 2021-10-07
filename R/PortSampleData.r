
PortSampleData <-function(sp=30,datadir="C:/Users/hubleyb/Documents/Halibut/data",by.sex=T,bins=seq(0,260,5),lwA=0.006803616,lwB=3.119924){

  # calculate bin weights
  binwts = lwA*(bins[-1]-diff(bins)/2)^lwB
  names(binwts)<-paste0("L",bins[-1])

  psdata<-get_ps_data(data.dir=datadir)
  samples<-subset(psdata,!duplicated(SAMPLE),which(!names(psdata)%in%c("SEX", "GROUPING", "LENGROUP", "NUMATLEN")))
  samples$GEAR<-NA
  samples$GEAR[samples$FISHING==3]<-"LL"
  samples$GEAR[samples$FISHING==7]<-"OT"
  samples$YEAR<-lubridate::year(samples$DATELANDED)

  sid=unique(psdata$SAMPLE)
  LF <-list()
  LFnosex<-data.frame('SAMPLE'=sid,t(sapply(sid,function(s){with(subset(psdata,SAMPLE==s),binNumAtLen(NUMATLEN,LENGROUP,bins))})))

  #names(LFnosex)[-1]<-paste0("L",bins[-1])
  LFnosex$NUM_MEASURED <- rowSums(LFnosex[paste0("L",bins[-1])],na.rm=T)
  LFnosex$WEIGHT_MEASURED <- rowSums(sweep(LFnosex[paste0("L",bins[-1])],2,binwts,"*"))/1000

  if(by.sex==T){
    psdata$SEX[psdata$SEX==9]<-0
    sx<-c(1,2,0)
    for(i in 1:3){
      LF[[i]]<-data.frame('SAMPLE'=sid,'SEXCD_ID'=sx[i],t(sapply(sid,function(s){with(subset(psdata,SAMPLE==s&SEX==sx[i]), binNumAtLen(NUMATLEN,LENGROUP,bins))})))
    }
    LF <- do.call("rbind",LF)
    #names(LF)[-(1:2)]<-paste0("L",bins[-1])

    LF <-merge(LF,LFnosex[,c('SAMPLE','NUM_MEASURED','WEIGHT_MEASURED')])
  }
  if(by.sex==F)LF <- LFnosex

  PORTSAMPLEDATA <- left_join(samples,LF)

  return(PORTSAMPLEDATA)

}
