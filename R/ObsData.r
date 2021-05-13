ObsData <-function(sp=30,datadir="C:/Users/hubleyb/Documents/Halibut/data",by.sex=T,bins=seq(0,260,5),lwA=0.006803616,lwB=3.119924){


  # calculate bin weights
  binwts = lwA*(bins[-1]-diff(bins)/2)^lwB
  names(binwts)<-paste0("L",bins[-1])

  # get observer data from ISDB
  isdb <- new.env()

  Mar.datawrangling::get_data(db='isdb',data.dir=datadir,env=isdb)

  # filter for targeded halibut and commercial index trips
  isdb$ISTRIPTYPECODES= isdb$ISTRIPTYPECODES[isdb$ISTRIPTYPECODES$TRIPCD_ID %in% c(12,30,7001,7057),]

  # filter for commercial and commercial index sets
  isdb$ISSETTYPECODES= isdb$ISSETTYPECODES[isdb$ISSETTYPECODES$SETCD_ID == c(1,10),]

  # filter for halibut
  isdb$ISSPECIESCODES= isdb$ISSPECIESCODES[isdb$ISSPECIESCODES$SPECCD_ID == 30,]

  # Apply filter
  Mar.datawrangling::self_filter('isdb',env=isdb)

  ## Trips
  trips <- left_join(isdb$ISTRIPS[,c("TRIP_ID","TRIPCD_ID","TRIP","VESS_ID")], isdb$ISVESSELS[,c("VESS_ID","VESSEL_NAME","CFV")], by='VESS_ID')


  ## Sets
  isdb$ISSETPROFILE_WIDE$DATE_TIME4[isdb$ISSETPROFILE_WIDE$DATE_TIME4>Sys.time()]<-NA
  isdb$ISSETPROFILE_WIDE$DATE_TIME1[isdb$ISSETPROFILE_WIDE$DATE_TIME1>Sys.time()]<-NA
  isdb$ISSETPROFILE_WIDE$SOAKMINP3P1 <- difftime(isdb$ISSETPROFILE_WIDE$DATE_TIME4,  isdb$ISSETPROFILE_WIDE$DATE_TIME1, units='min')
  isdb$ISSETPROFILE_WIDE$DEPTH <- rowMeans(isdb$ISSETPROFILE_WIDE[,c("DEP1","DEP2","DEP3","DEP4")],na.rm=T)
  sets <- left_join(isdb$ISSETPROFILE_WIDE[,c("FISHSET_ID","SET_NO","DATE_TIME1","DATE_TIME4","SOAKMINP3P1","DEPTH","LATITUDE","LONGITUDE","YEAR")], isdb$ISFISHSETS[,c("FISHSET_ID","TRIP_ID","SET_NO","SETCD_ID","NAFAREA_ID","NUM_HOOK_HAUL","GEAR_ID")], by=c('FISHSET_ID','SET_NO'))
  sets <- left_join(sets,isdb$ISGEARS[,c("GEAR_ID","TRIP_ID","GEARCD_ID", "HOOKCD_ID","HOOKSIZE")])
  sets$SOAKMINP3P1[sets$SOAKMINP3P1<0]<-NA

  ## Fish
  totalfish <- isdb$ISCATCHES[,c("FISHSET_ID","CATCH_ID","EST_NUM_CAUGHT","EST_COMBINED_WT")]

  # get length frequency
  Mar.datawrangling::get_data_custom(schema="observer", data.dir = datadir, tables = c("ISFISHLENGTHS","ISSAMPLES"))

  #The new tables get downloaded and/or loaded in and you can filter them manually
  ISSAMPLES = subset(ISSAMPLES,CATCH_ID %in% isdb$ISCATCHES$CATCH_ID,c("SMPL_ID","CATCH_ID","SEXCD_ID"))
  ISFISHLENGTHS=subset(ISFISHLENGTHS,SMPL_ID %in% ISSAMPLES$SMPL_ID,c("SMPL_ID","FISH_LENGTH","NUM_AT_LENGTH"))

  fishlengths <- left_join(ISSAMPLES,ISFISHLENGTHS)

  cid=unique(fishlengths$CATCH_ID)
  LF <-list()
  LFnosex<-data.frame('CATCH_ID'=cid,t(sapply(cid,function(s){with(subset(fishlengths,CATCH_ID==s),binNumAtLen(NUM_AT_LENGTH,FISH_LENGTH,bins))})))

  #names(LFnosex)[-1]<-paste0("L",bins[-1])
  LFnosex$NUM_MEASURED <- rowSums(LFnosex[paste0("L",bins[-1])],na.rm=T)
  LFnosex$WEIGHT_MEASURED <- rowSums(sweep(LFnosex[paste0("L",bins[-1])],2,binwts,"*"))/1000

  if(by.sex==T){
    sx<-c(1,2,0)
    for(i in 1:3){
      LF[[i]]<-data.frame('CATCH_ID'=cid,'SEXCD_ID'=sx[i],t(sapply(cid,function(s){with(subset(fishlengths,CATCH_ID==s&SEXCD_ID==sx[i]), binNumAtLen(NUM_AT_LENGTH,FISH_LENGTH,bins))})))
    }
    LF <- do.call("rbind",LF)
    #names(LF)[-(1:2)]<-paste0("L",bins[-1])

    LF <-merge(LF,LFnosex[,c('CATCH_ID','NUM_MEASURED','WEIGHT_MEASURED')])
  }
  if(by.sex==F)LF <- LFnosex

  totalfish <- left_join(totalfish,LF)

  OBSERVERDATA <- right_join(sets,totalfish) %>%
    left_join(.,trips)

  return(OBSERVERDATA)
  }




