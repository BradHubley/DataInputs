#' @export

RandomSurveyData <- function(sp=30, datadir, add.gear=F, add.LF=T, bins=seq(5,260,5), by.sex=T, hook.data=F, LF.from='ISFISHLENGTHS'){

  library(Mar.datawrangling)
  library(tidyverse)

  ## Survey data from database (isdb)

  isdb <- new.env()

  get_data(db='isdb',data.dir=datadir,env=isdb)

  # filter for halibut longline survey
  isdb$ISTRIPTYPECODES= isdb$ISTRIPTYPECODES[isdb$ISTRIPTYPECODES$TRIPCD_ID %in% c(7057),]

  # filter for random survey
  isdb$ISSETTYPECODES= isdb$ISSETTYPECODES[isdb$ISSETTYPECODES$SETCD_ID == 5,]

  # filter out bad sets
  isdb$ISFISHSETS= isdb$ISFISHSETS[isdb$ISFISHSETS$HAULCCD_ID %in% c(1,2,3),]

    # filter for halibut
  isdb$ISSPECIESCODES= isdb$ISSPECIESCODES[isdb$ISSPECIESCODES$SPECCD_ID == sp,]

  # Apply filter
  self_filter('isdb',env=isdb)

  #### Select relevent columns and Join tables into HALIBUTSURVEY

  ## Trips
  trips <- left_join(isdb$ISTRIPS[,c("TRIP_ID","TRIPCD_ID","TRIP","VESS_ID")], isdb$ISVESSELS[,c("VESS_ID","VESSEL_NAME","CFV")], by='VESS_ID')


  ## Sets
  isdb$ISSETPROFILE_WIDE$SOAKMINP3P1 <- difftime(isdb$ISSETPROFILE_WIDE$DATE_TIME3,  isdb$ISSETPROFILE_WIDE$DATE_TIME1, units='min')
  isdb$ISSETPROFILE_WIDE$SOAKMINP3P2 <- difftime(isdb$ISSETPROFILE_WIDE$DATE_TIME3,  isdb$ISSETPROFILE_WIDE$DATE_TIME2, units='min')
  isdb$ISFISHSETS$GEAR_LEN_M <- ((isdb$ISFISHSETS$LEN_LONGLINE)*1000)
  sets <- left_join(isdb$ISSETPROFILE_WIDE[,c("FISHSET_ID","SET_NO","DATE_TIME1","DATE_TIME2","DATE_TIME3","DATE_TIME4","SOAKMINP3P1","SOAKMINP3P2","LAT1","LAT2","LAT3","LAT4","LONG1","LONG2","LONG3","LONG4","DUR_41","YEAR","DEP1","DEP2","DEP3","DEP4")],
                    isdb$ISFISHSETS[,c("FISHSET_ID","TRIP_ID","SET_NO","SETCD_ID","STATION","STRATUM_ID","NAFAREA_ID","NUM_HOOK_HAUL","GEAR_ID","HAULCCD_ID","LEN_LONGLINE","GEAR_LEN_M","SPECSCD_ID")], by=c('FISHSET_ID','SET_NO'))
  # join gear if desired
  if(add.gear){
    ## Gear
    baitcodes<-isdb$ISGEARFEATURECODES$GEARFCD_ID[isdb$ISGEARFEATURECODES$GEARFCL_ID=="BAIT TYPES"]
    isdb$ISGEARFEATURES$FEATURE_VALUE[isdb$ISGEARFEATURES$GEARFCD_ID %in%baitcodes]<-1
    gear <- left_join(isdb$ISGEARFEATURES[,c("GEAR_ID","GEARFCD_ID","FEATURE_VALUE")], isdb$ISGEARFEATURECODES[,c("GEARFCD_ID","FEATURE")]) %>%
      select(.,GEAR_ID,FEATURE_VALUE,FEATURE) %>%
      pivot_wider(.,names_from = FEATURE,values_from = FEATURE_VALUE) %>%
      left_join(.,isdb$ISGEARS[,c("GEAR_ID","TRIP_ID","GEARCD_ID", "HOOKCD_ID","HOOKSIZE")])%>% data.frame()

    # fix names
    names(gear)<-gsub(".M.", "M", names(gear), fixed = TRUE)
    names(gear)<-gsub(".AVERAGE_KG.", "AVERAGE_KG", names(gear), fixed = TRUE)
    names(gear)<-gsub(".", "_", names(gear), fixed = TRUE)


    sets <- left_join(sets,select(gear,!c(MUSTAD)),by=c('GEAR_ID','TRIP_ID'))
  }

  ##### Fish

  totalfish <- isdb$ISCATCHES[,c("FISHSET_ID","CATCH_ID","EST_NUM_CAUGHT","EST_COMBINED_WT")]


  ### join length frequency if desired
  if(add.LF){
    if(LF.from=='ISFISH'){
      #bins<-seq(size.range[1],size.range[2],bin.size)
      cid=unique(isdb$ISFISH$CATCH_ID)
      LF <-list()
      LFnosex<-data.frame('CATCH_ID'=cid,t(sapply(cid,function(s){with(subset(isdb$ISFISH,CATCH_ID==s),hist(FISH_LENGTH,breaks=bins,plot=F,right=F)$count)})))
      names(LFnosex)[-1]<-paste0("L",bins[-1])
      LFnosex$NUM_MEASURED <- rowSums(LFnosex[,-1],na.rm=T)
      if(by.sex==T){
        sx<-c(1,2,0)
        for(i in 1:3){
          LF[[i]]<-data.frame('CATCH_ID'=cid,'SEXCD_ID'=sx[i],t(sapply(cid,function(s){with(subset(isdb$ISFISH,CATCH_ID==s&SEXCD_ID==sx[i]),hist(FISH_LENGTH,breaks=bins,plot=F,right=F)$count)})))
        }
        LF <- do.call("rbind",LF)
        names(LF)[-(1:2)]<-paste0("L",bins[-1])

        LF <-merge(LF,LFnosex[,c('CATCH_ID','NUM_MEASURED')])
      }
      if(by.sex==F)LF <- LFnosex
      #browser()
    }
    if(LF.from=="ISFISHLENGTHS"){
      Mar.datawrangling::get_data_custom(schema="observer", data.dir = datadir, tables = c("ISFISHLENGTHS","ISSAMPLES"),env=isdb)

      #The new tables get downloaded and/or loaded in and you can filter them manually
      ISSAMPLES = subset(isdb$ISSAMPLES,CATCH_ID %in% isdb$ISCATCHES$CATCH_ID,c("SMPL_ID","CATCH_ID","SEXCD_ID"))
      ISFISHLENGTHS=subset(isdb$ISFISHLENGTHS,SMPL_ID %in% ISSAMPLES$SMPL_ID,c("SMPL_ID","FISH_LENGTH","NUM_AT_LENGTH"))

      fishlengths <- left_join(ISSAMPLES,ISFISHLENGTHS)%>% group_by(CATCH_ID) %>% mutate(avglength=weighted.mean(FISH_LENGTH ,NUM_AT_LENGTH,na.rm=T))

      cid=unique(fishlengths$CATCH_ID)
      LF <-list()
      LFnosex<-data.frame('CATCH_ID'=cid,t(sapply(cid,function(s){with(subset(fishlengths,CATCH_ID==s),binNumAtLen(NUM_AT_LENGTH,FISH_LENGTH,bins))})))
      #names(LFnosex)[-1]<-paste0("L",bins[-1])
      LFnosex$NUM_MEASURED <- rowSums(LFnosex[,-1],na.rm=T)
      LFnosex <- merge(LFnosex,subset(fishlengths,!duplicated(CATCH_ID),c("CATCH_ID","avglength")))
      if(by.sex==T){
        sx<-c(1,2,0)
        for(i in 1:3){
          LF[[i]]<-data.frame('CATCH_ID'=cid,'SEXCD_ID'=sx[i],t(sapply(cid,function(s){with(subset(fishlengths,CATCH_ID==s&SEXCD_ID==sx[i]), binNumAtLen(NUM_AT_LENGTH,FISH_LENGTH,bins))})))
        }
        LF <- do.call("rbind",LF)
        #names(LF)[-(1:2)]<-paste0("L",bins[-1])

        LF <-merge(LF,LFnosex[,c('CATCH_ID','NUM_MEASURED')])
      }
      if(by.sex==F)LF <- LFnosex

    }
    totalfish <- left_join(totalfish,LF)
  } # end length frequency


  HALIBUTSURVEY <- left_join(sets,totalfish) %>%
    left_join(.,trips)

  # get assigned strata
  StnStrt<-read.csv(file.path(datadir,"Survey","HS_STATION_STRATA.csv"))
  HALIBUTSURVEY$ASSIGNED_STATION<-floor(as.numeric( HALIBUTSURVEY$STATION))
  HALIBUTSURVEY <- left_join(HALIBUTSURVEY,StnStrt)
  HALIBUTSURVEY$STRATUM_ID <- HALIBUTSURVEY$ASSIGNED_STRATUM_ID

  if(hook.data==T){
    hookData<-PrepareDataHookModel(datadir = datadir)
    hooknames <- c("FISHSET_ID", "broken_hook", "empty_baited", "empty_unbaited", "other_species", "target_species", "missing_hook", "total_sampled", "ASSIGNED_STATION", "ASSIGNED_STRATUM_ID")
    HALIBUTSURVEY <-left_join(HALIBUTSURVEY,hookData[,hooknames])
  }



write.csv(HALIBUTSURVEY,file.path(datadir,"RandomHalibutSurveyData_LF.csv"),row.names = F)
return(HALIBUTSURVEY)

}



