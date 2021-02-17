#---
#  title: "prepare hook data for spatial model"
#author: "Brad"
#date: "November 10, 2020"
#output: word_document
#---

FixedSurveyData <-function(sp=30,datadir="C:/Users/hubleyb/Documents/Halibut/data",add.gear=F,add.LF=T,bins=seq(5,260,5),by.sex=T){

  library(Mar.datawrangling)
  library(tidyverse)

  ## Survey data from database (isdb)

  isdb <- new.env()

  get_data(db='isdb',data.dir=datadir,env=isdb)

  # filter for halibut longline survey
  isdb$ISTRIPTYPECODES= isdb$ISTRIPTYPECODES[isdb$ISTRIPTYPECODES$TRIPCD_ID %in% c(7057),]

  # filter for random survey
  isdb$ISSETTYPECODES= isdb$ISSETTYPECODES[isdb$ISSETTYPECODES$SETCD_ID == 4,]

  # filter out bad sets
  isdb$ISFISHSETS= isdb$ISFISHSETS[isdb$ISFISHSETS$HAULCCD_ID %in% c(1,2,3),]


  # filter for halibut
  isdb$ISSPECIESCODES= isdb$ISSPECIESCODES[isdb$ISSPECIESCODES$SPECCD_ID == sp,]


  # Apply filter
  self_filter('isdb',env=isdb)

  #### Select relevent columns and Join tables into HALIBUTSURVEY

  ## Trips
  trips <- left_join(select(isdb$ISTRIPS,TRIP_ID,TRIPCD_ID,TRIP,VESS_ID), select(isdb$ISVESSELS,VESS_ID,VESSEL_NAME,CFV), by='VESS_ID')


  ## Sets
  isdb$ISSETPROFILE_WIDE$SOAKMINP3P1 <- difftime(isdb$ISSETPROFILE_WIDE$DATE_TIME3,  isdb$ISSETPROFILE_WIDE$DATE_TIME1, units='min')
  isdb$ISSETPROFILE_WIDE$DEPTH <- rowMeans(select(isdb$ISSETPROFILE_WIDE,DEP1,DEP2,DEP3,DEP4),na.rm=T)
  sets <- left_join(select(isdb$ISSETPROFILE_WIDE,FISHSET_ID,SET_NO,DATE_TIME3,SOAKMINP3P1,DEPTH,LATITUDE,LONGITUDE,YEAR), select(isdb$ISFISHSETS,FISHSET_ID,TRIP_ID,SET_NO,SETCD_ID,STATION,STRATUM_ID,NAFAREA_ID,NUM_HOOK_HAUL,GEAR_ID), by=c('FISHSET_ID','SET_NO'))

  # join gear if desired
  if(add.gear){
    ## Gear
    gear <- left_join(select(isdb$ISGEARFEATURES,GEAR_ID,GEARFCD_ID,FEATURE_VALUE), select(isdb$ISGEARFEATURECODES,GEARFCD_ID,FEATURE)) %>%
      select(.,GEAR_ID,FEATURE_VALUE,FEATURE) %>%
      pivot_wider(.,names_from = FEATURE,values_from = FEATURE_VALUE) %>%
      left_join(.,select(isdb$ISGEARS,GEAR_ID,TRIP_ID,GEARCD_ID, HOOKCD_ID,HOOKSIZE))%>% data.frame()

    # fix names
    names(gear)<-gsub(".M.", "M", names(gear), fixed = TRUE)
    names(gear)<-gsub(".AVERAGE_KG.", "AVERAGE_KG", names(gear), fixed = TRUE)
    names(gear)<-gsub(".", "_", names(gear), fixed = TRUE)


    sets <- left_join(sets,select(gear,!c(HERRING,MUSTAD)),by=c('GEAR_ID','TRIP_ID'))
  }

  ## Fish

  totalfish <- select(isdb$ISCATCHES,FISHSET_ID,CATCH_ID,EST_NUM_CAUGHT,EST_COMBINED_WT)

  # join length frequency if desired
  if(add.LF){
    #bins<-seq(size.range[1],size.range[2],bin.size)
    cid=unique(isdb$ISFISH$CATCH_ID)
    LF <-list()
    LFnosex<-data.frame('CATCH_ID'=cid,t(sapply(cid,function(s){with(subset(isdb$ISFISH,CATCH_ID==s),hist(FISH_LENGTH,breaks=bins,plot=F)$count)})))
    names(LFnosex)[-1]<-paste0("L",bins[-1])
    LFnosex$NUM_MEASURED <- rowSums(LFnosex[,-1],na.rm=T)
    if(by.sex==T){
      sx<-c(1,2,0)
      for(i in 1:3){
        LF[[i]]<-data.frame('CATCH_ID'=cid,'SEXCD_ID'=sx[i],t(sapply(cid,function(s){with(subset(isdb$ISFISH,CATCH_ID==s&SEXCD_ID==sx[i]),hist(FISH_LENGTH,breaks=bins,plot=F)$count)})))
      }
      LF <- do.call("rbind",LF)
      names(LF)[-(1:2)]<-paste0("L",bins[-1])

      LF <-merge(LF,LFnosex[,c('CATCH_ID','NUM_MEASURED')])
    }
    if(by.sex==F)LF <- LFnosex
    #browser()
    totalfish <- left_join(totalfish,LF)
  }


  HALIBUTSURVEY <- left_join(sets,totalfish) %>%
    left_join(.,trips)



write.csv(HALIBUTSURVEY,file.path(datadir,"FixedHalibutSurveyData.csv"),row.names = F)
return(HALIBUTSURVEY)
}


