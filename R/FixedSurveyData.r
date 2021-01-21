#---
#  title: "prepare hook data for spatial model"
#author: "Brad"
#date: "November 10, 2020"
#output: word_document
#---
FixedSurveyData <-function(sp=30,wd="C:/Users/hubleyb/Documents/Halibut/data",add.gear=F,add.LF=T,bin.size=5,size.range=c(5,250)){

  library(Mar.datawrangling)
  library(tidyverse)

  ## Survey data from database (isdb)

  isdb <- new.env()

  get_data(db='isdb',data.dir=wd,env=isdb)

  attach(isdb)
  # filter for halibut longline survey
  ISTRIPTYPECODES= ISTRIPTYPECODES[ISTRIPTYPECODES$TRIPCD_ID %in% c(7057),]

  # filter for random survey
  ISSETTYPECODES= ISSETTYPECODES[ISSETTYPECODES$SETCD_ID == 4,]

  # filter out bad sets
  ISFISHSETS= ISFISHSETS[ISFISHSETS$HAULCCD_ID %in% c(1,2,3),]


  # filter for halibut
  ISSPECIESCODES= ISSPECIESCODES[ISSPECIESCODES$SPECCD_ID == sp,]


  # Apply filter
  self_filter('isdb')

  #### Select relevent columns and Join tables into HALIBUTSURVEY

  ## Trips
  trips <- left_join(select(ISTRIPS,TRIP_ID,TRIPCD_ID,TRIP,VESS_ID), select(ISVESSELS,VESS_ID,VESSEL_NAME,CFV), by='VESS_ID')


  ## Sets
  ISSETPROFILE_WIDE$SOAKMINP3P1 <- difftime(ISSETPROFILE_WIDE$DATE_TIME3,  ISSETPROFILE_WIDE$DATE_TIME1, units='min')
  ISSETPROFILE_WIDE$DEPTH <- rowMeans(select(ISSETPROFILE_WIDE,DEP1,DEP2,DEP3,DEP4),na.rm=T)
  sets <- left_join(select(ISSETPROFILE_WIDE,FISHSET_ID,SET_NO,DATE_TIME3,SOAKMINP3P1,DEPTH,LATITUDE,LONGITUDE,YEAR), select(ISFISHSETS,FISHSET_ID,TRIP_ID,SET_NO,SETCD_ID,STATION,STRATUM_ID,NAFAREA_ID,NUM_HOOK_HAUL,GEAR_ID), by=c('FISHSET_ID','SET_NO'))

  # join gear if desired
  if(add.gear){
    ## Gear
    gear <- left_join(select(ISGEARFEATURES,GEAR_ID,GEARFCD_ID,FEATURE_VALUE), select(ISGEARFEATURECODES,GEARFCD_ID,FEATURE)) %>%
      select(.,GEAR_ID,FEATURE_VALUE,FEATURE) %>%
      pivot_wider(.,names_from = FEATURE,values_from = FEATURE_VALUE) %>%
      left_join(.,select(ISGEARS,GEAR_ID,TRIP_ID,GEARCD_ID, HOOKCD_ID,HOOKSIZE))%>% data.frame()

    # fix names
    names(gear)<-gsub(".M.", "M", names(gear), fixed = TRUE)
    names(gear)<-gsub(".AVERAGE_KG.", "AVERAGE_KG", names(gear), fixed = TRUE)
    names(gear)<-gsub(".", "_", names(gear), fixed = TRUE)


    sets <- left_join(sets,select(gear,!c(HERRING,MUSTAD)),by=c('GEAR_ID','TRIP_ID'))
  }

  ## Fish

  totalfish <- select(ISCATCHES,FISHSET_ID,CATCH_ID,EST_NUM_CAUGHT,EST_COMBINED_WT)

  # join length frequency if desired
  if(add.LF){
    bins<-seq(size.range[1],size.range[2],bin.size)
    cid=unique(ISFISH$CATCH_ID)

    LF<-data.frame('CATCH_ID'=cid,t(sapply(cid,function(s){with(subset(ISFISH,CATCH_ID==s),hist(FISH_LENGTH,breaks=bins,plot=F)$count)})))
    names(LF)[-1]<-paste0("L",bins[-1])
    totalfish <- left_join(totalfish,LF)
  }

  detach(isdb)

  HALIBUTSURVEY <- left_join(sets,totalfish) %>%
    left_join(.,trips)



write.csv(HALIBUTSURVEY,file.path(wd,"FixedHalibutSurveyData.csv"),row.names = F)
return(HALIBUTSURVEY)
}


