getMARFISdata <- function(datadir,sp=130,gear=1){

  marfis <- new.env()
  get_data(db='marfis',data.dir=datadir,env=marfis)


  nafo3N = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("3N",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
  nafo3O = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("3O",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
  nafo3PS = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("3PS",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
  nafo4VN = "4VN"
  nafo4VS = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("4VS",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
  nafo4W = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("4W",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
  nafo4X = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("4X",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
  nafo5Y = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("5Y",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]
  nafo5Z = marfis$NAFO_UNIT_AREAS$NAFO_AREA[grep("5Z",marfis$NAFO_UNIT_AREAS$NAFO_AREA)]

  marfisNAFOs = c(nafo3N,nafo3O,nafo3PS,nafo4VN,nafo4VS,nafo4W,nafo4X,nafo5Y,nafo5Z)


  marfis$SPECIES = marfis$SPECIES[marfis$SPECIES$SPECIES_CODE == sp,]
  marfis$NAFO_UNIT_AREAS = marfis$NAFO_UNIT_AREAS[marfis$NAFO_UNIT_AREAS$NAFO_AREA%in%marfisNAFOs,]
  #VESSELS = VESSELS[VESSELS$LOA<45,]
  marfis$GEARS = marfis$GEARS[marfis$GEARS$GEAR_TYPE_ID==gear,]
  #PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$VR_NUMBER_FISHING %in% VESSELS$VR_NUMBER | PRO_SPC_INFO$VR_NUMBER_LANDING %in% VESSELS$VR_NUMBER, ]

  self_filter('marfis',env=marfis)

  marfis$NAFO_UNIT_AREAS$NAFO = marfis$NAFO_UNIT_AREAS$NAFO_AREA
  marfis$NAFO_UNIT_AREAS$NAFO[marfis$NAFO_UNIT_AREAS$NAFO_AREA%in%nafo3N] = "3N"
  marfis$NAFO_UNIT_AREAS$NAFO[marfis$NAFO_UNIT_AREAS$NAFO_AREA%in%nafo3O] = "3O"
  marfis$NAFO_UNIT_AREAS$NAFO[marfis$NAFO_UNIT_AREAS$NAFO_AREA%in%nafo3PS] = "3PS"
  marfis$NAFO_UNIT_AREAS$NAFO[marfis$NAFO_UNIT_AREAS$NAFO_AREA%in%nafo4VS] = "4VS"
  marfis$NAFO_UNIT_AREAS$NAFO[marfis$NAFO_UNIT_AREAS$NAFO_AREA%in%nafo4W] = "4W"
  marfis$NAFO_UNIT_AREAS$NAFO[marfis$NAFO_UNIT_AREAS$NAFO_AREA%in%nafo4X] = "4X"
  marfis$NAFO_UNIT_AREAS$NAFO[marfis$NAFO_UNIT_AREAS$NAFO_AREA%in%nafo5Y] = "5Y"
  marfis$NAFO_UNIT_AREAS$NAFO[marfis$NAFO_UNIT_AREAS$NAFO_AREA%in%nafo5Z] = "5Z"

  colm.names<-c("YEAR","NAFO_UNIT_AREA_ID","RND_WEIGHT_KGS","LATITUDE","LONGITUDE","VR_NUMBER_FISHING","DATE_FISHED","TRIP_ID")

  MarfisData = left_join(marfis$PRO_SPC_INFO[,colm.names],marfis$NAFO_UNIT_AREAS[,c("NAFO","AREA_ID")],by=c("NAFO_UNIT_AREA_ID"="AREA_ID")) %>%
    left_join(.,VESSELS[,c("LOA","VR_NUMBER")],by=c("VR_NUMBER_FISHING"="VR_NUMBER"))

  return(MarfisData)

}
