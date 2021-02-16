#---
#  title: "prepare hook data for spatial model"
#author: "Brad"
#date: "November 10, 2020"
#output: word_document
#---

PrepareDataHookModel <-function(sp=30,datadir="C:/Users/hubleyb/Documents/Halibut/data",add.gear=F){


  library(Mar.datawrangling)
  library(tidyverse)
  #library(bio.halibut)
  #wd="C:/Users/hubleyb/Documents/Halibut/git/bio.halibut"
  #sp=30

  ## Survey data from database (isdb)xc
  ## Hook data from flat files



  ## Get Hook data from flat files
  hook_data <- hookData(wdir=datadir, species=sp)

  # Get Halibut Survey from ISDB
  #get_data(db='isdb',data.dir=file.path(wd,'data'),fn.oracle.username = "hubleyb", fn.oracle.password = "R4#vmxtas", fn.oracle.dsn = 'ptran',usepkg='roracle',force.extract=T)

  isdb <- new.env()

  get_data(db='isdb',data.dir=datadir,env=isdb)

  # filter for halibut longline survey
  isdb$ISTRIPTYPECODES= isdb$ISTRIPTYPECODES[isdb$ISTRIPTYPECODES$TRIPCD_ID %in% c(7057,7058),]

  # filter for specific trips
  #ISTRIPS = ISTRIPS[ISTRIPS$TRIP%in%missed.trips,]

  # Apply filter
  self_filter('isdb',env=isdb)

  # calculate avg weight by species from longline survey (used later)
  fish.avg.weight <- select(isdb$ISCATCHES,FISHSET_ID,CATCH_ID,SPECCD_ID,EST_NUM_CAUGHT,EST_COMBINED_WT) %>%
    mutate(AVG_WEIGHT = EST_COMBINED_WT/EST_NUM_CAUGHT ) %>%
    group_by(.,SPECCD_ID) %>%
    summarise(mean_AVG_WEIGHT=mean(AVG_WEIGHT,na.rm=T))

  # filter for halibut longline survey
  isdb$ISTRIPTYPECODES= isdb$ISTRIPTYPECODES[isdb$ISTRIPTYPECODES$TRIPCD_ID %in% c(7057),]

  # filter for random survey
  isdb$ISSETTYPECODES= isdb$ISSETTYPECODES[isdb$ISSETTYPECODES$SETCD_ID == 5,]

  # filter out bad sets
  isdb$ISFISHSETS= isdb$ISFISHSETS[isdb$ISFISHSETS$HAULCCD_ID %in% c(1,2,3),]

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

  # fill in NAs in NuM_CAUGHT with estimate from COMBINED_WT
  fish <- left_join(select(isdb$ISCATCHES,FISHSET_ID,CATCH_ID,SPECCD_ID,EST_NUM_CAUGHT,EST_COMBINED_WT),fish.avg.weight) %>%
     mutate(EST_NUM_FROM_WEIGHT = round(EST_COMBINED_WT/mean_AVG_WEIGHT))
  #summary(lm(EST_NUM_FROM_WEIGHT~EST_NUM_CAUGHT-1,fish))
  fish$EST_NUM_CAUGHT[is.na(fish$EST_NUM_CAUGHT)]<-fish$EST_NUM_FROM_WEIGHT[is.na(fish$EST_NUM_CAUGHT)]
  fish$EST_NUM_CAUGHT[fish$EST_NUM_CAUGHT==0]<-1


  targetfish <- subset(fish,SPECCD_ID==sp,c('FISHSET_ID','EST_NUM_CAUGHT'))
  names(targetfish)[2]<-'total_target_species'
  allfish <- filter(fish,SPECCD_ID!=sp) %>%
    group_by(.,FISHSET_ID) %>%
    summarise(total_other_species=sum(EST_NUM_CAUGHT,na.rm=T)) %>%
    full_join(.,targetfish)
  allfish[is.na(allfish)]<-0

  HALIBUTSURVEY <- left_join(sets,allfish) %>%
    left_join(.,trips) %>%
    left_join(.,hook_data)

  HALIBUTSURVEY$total_other_species[is.na(HALIBUTSURVEY$total_other_species)]<-0
  HALIBUTSURVEY$total_target_species[is.na(HALIBUTSURVEY$total_target_species)]<-0

  # get assigned strata
  StnStrt<-read.csv(file.path(wd,"data","HS_STATION_STRATA.csv"))
  HALIBUTSURVEY$ASSIGNED_STATION<-floor(as.numeric( HALIBUTSURVEY$STATION))
  HALIBUTSURVEY <- left_join(HALIBUTSURVEY,StnStrt)
  HALIBUTSURVEY$STRATUM_ID <- HALIBUTSURVEY$ASSIGNED_STRATUM_ID



write.csv(HALIBUTSURVEY,file.path(datadir,"HalibutSurveyHookData.csv"),row.names = F)
return(HALIBUTSURVEY)
}


