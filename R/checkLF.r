
checkLF<-function(datadir,sp=30){

  isdb <- new.env()

  Mar.datawrangling::get_data(db='isdb',data.dir=datadir,env=isdb)

  # filter for halibut longline survey
  isdb$ISTRIPTYPECODES= isdb$ISTRIPTYPECODES[isdb$ISTRIPTYPECODES$TRIPCD_ID %in% c(7057),]


  # filter for halibut
  isdb$ISSPECIESCODES= isdb$ISSPECIESCODES[isdb$ISSPECIESCODES$SPECCD_ID == sp,]


  # Apply filter
  Mar.datawrangling::self_filter('isdb',env=isdb)

  ## Trips
  trips <- left_join(isdb$ISTRIPS[,c("TRIP_ID","TRIPCD_ID","TRIP","VESS_ID")], isdb$ISVESSELS[,c("VESS_ID","VESSEL_NAME","CFV")], by='VESS_ID')

  ## Sets
  sets <- left_join(isdb$ISSETPROFILE_WIDE[,c("FISHSET_ID","SET_NO","DATE_TIME3","LATITUDE","LONGITUDE","YEAR")], isdb$ISFISHSETS[,c("FISHSET_ID","TRIP_ID","SET_NO","SETCD_ID","STATION")], by=c('FISHSET_ID','SET_NO'))

  totalfish <- isdb$ISCATCHES[,c("FISHSET_ID","CATCH_ID","EST_NUM_CAUGHT","EST_COMBINED_WT")]

  Mar.datawrangling::get_data_custom(schema="observer", data.dir = datadir, tables = c("ISFISHLENGTHS","ISSAMPLES"))

  ISSAMPLES = subset(ISSAMPLES,CATCH_ID %in% isdb$ISCATCHES$CATCH_ID,c("SMPL_ID","CATCH_ID","SEXCD_ID"))
  ISFISHLENGTHS=subset(ISFISHLENGTHS,SMPL_ID %in% ISSAMPLES$SMPL_ID,c("SMPL_ID","FISH_LENGTH","NUM_AT_LENGTH"))

  fishlengths <- left_join(ISSAMPLES,ISFISHLENGTHS)

  fish <- isdb$ISFISH %>% group_by(CATCH_ID,SEXCD_ID,FISH_LENGTH) %>%
    summarize(ISFISH_COUNT=length(FISH_LENGTH)) %>%
    right_join(.,totalfish) %>%
    left_join(.,fishlengths)

  HALIBUTSURVEY <- left_join(sets,fish) %>%
    left_join(.,trips)

  HALIBUTSURVEY$ISFISH_COUNT[is.na(HALIBUTSURVEY$ISFISH_COUNT)]<-0
  HALIBUTSURVEY$NUM_AT_LENGTH[is.na(HALIBUTSURVEY$NUM_AT_LENGTH)]<-0

  HALIBUTSURVEY$MATCH <- HALIBUTSURVEY$ISFISH_COUNT == HALIBUTSURVEY$NUM_AT_LENGTH

  write.csv(HALIBUTSURVEY,"HalibutSurveyLFcheck.csv")

  return(HALIBUTSURVEY)

}

