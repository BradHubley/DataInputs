#---
#  title: "prepare hook data for spatial model"
#author: "Brad"
#date: "November 10, 2020"
#output: word_document
#---

FixedSurveyData <-function(sp=30, datadir="C:/Users/hubleyb/Documents/Halibut/data", add.gear=F, add.LF=T, bins=seq(5,260,5), by.sex=T, LF.from='ISFISHLENGTHS',correct.splitsets=T){

  library(Mar.datawrangling)
  library(tidyverse)

  ## Survey data from database (isdb)

  isdb <- new.env()

  Mar.datawrangling::get_data(db='isdb',data.dir=datadir,env=isdb)

  # filter for halibut longline survey
  isdb$ISTRIPTYPECODES= isdb$ISTRIPTYPECODES[isdb$ISTRIPTYPECODES$TRIPCD_ID %in% c(7057),]

  # filter for random survey
  isdb$ISSETTYPECODES= isdb$ISSETTYPECODES[isdb$ISSETTYPECODES$SETCD_ID == 4,]

  # filter out bad sets
  isdb$ISFISHSETS= isdb$ISFISHSETS[isdb$ISFISHSETS$HAULCCD_ID %in% c(1,2,3),]


  # filter for halibut
  isdb$ISSPECIESCODES= isdb$ISSPECIESCODES[isdb$ISSPECIESCODES$SPECCD_ID == sp,]


  # Apply filter
  Mar.datawrangling::self_filter('isdb',env=isdb)




  #### Select relevent columns and Join tables into HALIBUTSURVEY

  ## Trips
  trips <- left_join(isdb$ISTRIPS[,c("TRIP_ID","TRIPCD_ID","TRIP","VESS_ID")], isdb$ISVESSELS[,c("VESS_ID","VESSEL_NAME","CFV")], by='VESS_ID')


  ## Sets
  isdb$ISSETPROFILE_WIDE$SOAKMINP3P1 <- difftime(isdb$ISSETPROFILE_WIDE$DATE_TIME3,  isdb$ISSETPROFILE_WIDE$DATE_TIME1, units='min')
  isdb$ISSETPROFILE_WIDE$DEPTH <- rowMeans(isdb$ISSETPROFILE_WIDE[,c("DEP1","DEP2","DEP3","DEP4")],na.rm=T)
  sets <- left_join(isdb$ISSETPROFILE_WIDE[,c("FISHSET_ID","SET_NO","DATE_TIME1","DATE_TIME3","SOAKMINP3P1","DEPTH","LATITUDE","LONGITUDE","YEAR")], isdb$ISFISHSETS[,c("FISHSET_ID","TRIP_ID","SET_NO","SETCD_ID","STATION","STRATUM_ID","NAFAREA_ID","NUM_HOOK_HAUL","HAULCCD_ID","GEAR_ID")], by=c('FISHSET_ID','SET_NO'))

  sets$SOAKMINP3P1[sets$SOAKMINP3P1<0]<-NA

  # join gear if desired
  if(add.gear){
    ## Gear
    gear <- left_join(isdb$ISGEARFEATURES[,c("GEAR_ID","GEARFCD_ID","FEATURE_VALUE")], isdb$ISGEARFEATURECODES[,c("GEARFCD_ID","FEATURE")]) %>%
      select(.,GEAR_ID,FEATURE_VALUE,FEATURE) %>%
      pivot_wider(.,names_from = FEATURE,values_from = FEATURE_VALUE) %>%
      left_join(.,isdb$ISGEARS[,c("GEAR_ID","TRIP_ID","GEARCD_ID", "HOOKCD_ID","HOOKSIZE")])%>% data.frame()

    # fix names
    names(gear)<-gsub(".M.", "M", names(gear), fixed = TRUE)
    names(gear)<-gsub(".AVERAGE_KG.", "AVERAGE_KG", names(gear), fixed = TRUE)
    names(gear)<-gsub(".", "_", names(gear), fixed = TRUE)


    sets <- left_join(sets,select(gear,!c(HERRING,MUSTAD)),by=c('GEAR_ID','TRIP_ID'))
  }

  ## Fish

  totalfish <- isdb$ISCATCHES[,c("FISHSET_ID","CATCH_ID","EST_NUM_CAUGHT","EST_COMBINED_WT")]

  # join length frequency if desired
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
      #LFisfish <-LF
    }
    if(LF.from=="ISFISHLENGTHS"){
      Mar.datawrangling::get_data_custom(schema="observer", data.dir = datadir, tables = c("ISFISHLENGTHS","ISSAMPLES"))

      #The new tables get downloaded and/or loaded in and you can filter them manually
      ISSAMPLES = subset(ISSAMPLES,CATCH_ID %in% isdb$ISCATCHES$CATCH_ID,c("SMPL_ID","CATCH_ID","SEXCD_ID"))
      ISFISHLENGTHS=subset(ISFISHLENGTHS,SMPL_ID %in% ISSAMPLES$SMPL_ID,c("SMPL_ID","FISH_LENGTH","NUM_AT_LENGTH"))

      fishlengths <- left_join(ISSAMPLES,ISFISHLENGTHS)

      cid=unique(isdb$ISFISH$CATCH_ID)
      LF <-list()
      LFnosex<-data.frame('CATCH_ID'=cid,t(sapply(cid,function(s){with(subset(fishlengths,CATCH_ID==s),binNumAtLen(NUM_AT_LENGTH,FISH_LENGTH,bins))})))
      #names(LFnosex)[-1]<-paste0("L",bins[-1])
      LFnosex$NUM_MEASURED <- rowSums(LFnosex[,-1],na.rm=T)
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
      #browser()
      #LFisfishlengths <-LF

    }



    totalfish <- left_join(totalfish,LF)
  }


  HALIBUTSURVEY <- left_join(sets,totalfish) %>%
    left_join(.,trips)

  ## correct for splitsets
  if(correct.splitsets){
    if(by.sex==F){
    HALIBUTSURVEY$SY <- paste0(HALIBUTSURVEY$YEAR,HALIBUTSURVEY$STATION)
    dr<-subset(HALIBUTSURVEY,duplicated(SY))

    fsdups<-subset(HALIBUTSURVEY,SY%in%dr$SY)

    newd<-fsdups %>%
      group_by(YEAR,STATION,VESS_ID ) %>%
      summarise(SOAKMINP3P1=sum(SOAKMINP3P1),NUM_HOOK_HAUL=sum(NUM_HOOK_HAUL),across(EST_NUM_CAUGHT:NUM_MEASURED, sum, na.rm=T)) %>%
      right_join(.,dplyr::select(dr,YEAR,STATION,which(!names(dr)%in%names(.)))) %>%
      filter(!duplicated(SY)) %>%
      data.frame()

    HALIBUTSURVEY <- left_join(subset(HALIBUTSURVEY,!duplicated(SY)),newd)
    }
    if(by.sex==T){
      HALIBUTSURVEY$SYS <- paste0(HALIBUTSURVEY$YEAR,HALIBUTSURVEY$STATION,HALIBUTSURVEY$SEXCD_ID)
      dr<-subset(HALIBUTSURVEY,duplicated(SYS))

      fsdups<-subset(HALIBUTSURVEY,SYS%in%dr$SYS)

      newd<-fsdups %>%
        group_by(YEAR,STATION,VESS_ID,SEXCD_ID ) %>%
        summarise(SOAKMINP3P1=sum(SOAKMINP3P1),NUM_HOOK_HAUL=sum(NUM_HOOK_HAUL),across(EST_NUM_CAUGHT:NUM_MEASURED, sum, na.rm=T)) %>%
        right_join(.,dplyr::select(dr,YEAR,STATION,SEXCD_ID,which(!names(dr)%in%names(.)))) %>%
        filter(!duplicated(SYS)) %>%
        data.frame()

      HALIBUTSURVEY <- left_join(subset(HALIBUTSURVEY,!duplicated(SYS)),newd)
    }

  }





write.csv(HALIBUTSURVEY,file.path(datadir,"FixedHalibutSurveyData.csv"),row.names = F)
return(HALIBUTSURVEY)
}


