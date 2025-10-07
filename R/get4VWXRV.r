#' @export
get4VWXRV<-function(uid, pwd, use.local=T,datadir=datadir){

  if(use.local==F){


    # Make connection to Database PTRAN
    cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), uid, pwd, "PTRAN")

    # load Tables
    Mar.datawrangling::get_data(db='rv',cxn=cxn,force.extract=T)


  set.names<- c("MISSION","YEAR","SETNO", "SDATE", "TIME", "STRAT","STRATA_AREA","DIST","GEAR","WIDTH","VESEL","TYPE", "LATITUDE", "LONGITUDE","DEPTH","BOTTOM_TEMPERATURE" )
    sets<-merge(GSINF[GSINF$TYPE==1,],GSMISSIONS[GSMISSIONS$SEASON=="SUMMER",])
    sets$WIDTH = ifelse(sets$GEAR==3,10.97/1000,ifelse(sets$GEAR==9,12.49/1000,ifelse(sets$GEAR==15,13/1000,NA))) #yankee 36

    sets<-merge(sets,with(GSSTRATUM,data.frame(STRAT,STRATA_AREA=AREA*3.429904)),all.x=T) #strata area in km2
    sets<-merge(sets[,set.names],GSCAT[GSCAT$SPEC==30,],all.x=T)
    sets$TOTWGT[is.na(sets$TOTWGT)]<-0
    sets$TOTNO[is.na(sets$TOTNO)]<-0

    strata<-as.character(440:495)
    sets <- subset(sets,STRAT%in% strata)

    det.names<- c("MISSION","SETNO", "FLEN","CLEN","FWT", "FSEX" )
    fishlength <- merge(GSDET[GSDET$SPEC==30,det.names],sets)

       # Dist adjust
    fishlength$NUMATLEN<- fishlength$CLEN / fishlength$DIST

    # calculate density / km2
    fishlength$NUMATLEN <-   fishlength$NUMATLEN / 1.852 / fishlength$WIDTH

    lwA=0.006803616
    lwB=3.119924

    fishlength$WGTATLEN <- fishlength$FLEN^lwB * lwA * fishlength$NUMATLEN / 1000
    sets<-merge(sets,aggregate(cbind(NUMATLEN,WGTATLEN)~MISSION+YEAR+SETNO, fishlength, sum),all.x=T)
    sets<-sets[order(sets$YEAR,sets$SETNO),]

    sets$DENSITY_N <- sets$TOTNO / sets$DIST / 1.852 / sets$WIDTH
    sets$DENSITY_W <- sets$TOTWGT/ sets$DIST / 1.852 / sets$WIDTH
    sets$WGTATLEN[is.na(sets$WGTATLEN)]<-0
    sets$NUMATLEN[is.na(sets$NUMATLEN)]<-0
    sets$DENSITY_N[is.na(sets$DENSITY_N)]<-0
    sets$DENSITY_W[is.na(sets$DENSITY_W)]<-0

    sets$TOTWGT <- sets$TOTWGT*1.75 / sets$DIST
    sets$TOTNO <- sets$TOTNO*1.75 / sets$DIST
    sets$TUNITS <- sets$STRATA_AREA / sets$WIDTH * 0.3087579 # to get tunits similar to old code -BH

    # old code

    # Get fish length data - use statement provided by Nell
    #fishlength_get <- ROracle::dbSendQuery(conn = con, statement = "select i.mission,i.setno,(dmin+dmax)/2 depth, strat, latitude, longitude, sdate, dist,flen,fwt,clen, fsex from groundfish.gsinf i, groundfish.gsdet c where i.mission=c.mission and i.setno=c.setno and spec=30 and to_char(sdate,'mm') in ('06','07','08') and strat between '440' and '495'  and type=1")

    # fetch the data
    #fishlength <- ROracle::fetch(fishlength_get)

    # get the required set information - requires three queries
    # Get tows with halibut - only summer survey (months 6, 7, 8)
    #halibut_get <- ROracle::dbSendQuery(conn = con, statement = "select i.mission,i.setno,(dmin+dmax)/2 depth, strat, latitude, longitude, sdate, dist,totno,totwgt from groundfish.gsinf i, groundfish.gscat c where i.mission=c.mission and i.setno=c.setno and spec=30 and to_char(sdate,'mm') in ('06','07','08') and strat between '440' and '495'  and type=1")
    #halibut <- ROracle::fetch(halibut_get)

    # get tows with no halibut - only summer survey (months 6, 7, 8)
    #no_halibut_get <- ROracle::dbSendQuery(conn = con, statement = "select i.mission,i.setno,(dmin+dmax)/2 depth, strat, latitude, longitude, sdate,dist, 0 totno, 0 totwgt from groundfish.gsinf i where to_char(sdate,'mm') in ('06','07','08') and strat between '440' and '495' and type=1")
    #no_halibut <- ROracle::fetch(no_halibut_get)

    # Get stratum data
    #stratum_get <- ROracle::dbSendQuery(conn = con, statement = "select strat,area from groundfish.gsstratum")
    #stratum <- ROracle::fetch(stratum_get)

    #calculate the total weight and total numbers
    #halibut$TOTWGT <- halibut$TOTWGT*1.75 / halibut$DIST
    #halibut$TOTNO <- halibut$TOTNO*1.75 / halibut$DIST

    ## Merge the tows with and without halibut
    #allfish <- rbind(halibut, no_halibut)
    #allfish <- allfish[!duplicated(allfish[, c("MISSION", "SETNO")]),]

    # convert depth to metres
    #allfish$DEPTH <- allfish$DEPTH*1.8288

    # duplicate the stratum data frame - seems to have to do with how areas were measured before and after 1981 (different width of gears)
    #stratum2 <- stratum
    #stratum$TUNITS <- stratum$AREA / ((35/6080.2)*1.75)
    #stratum2$TUNITS <- stratum2$AREA / ((41/6080.2)*1.75)

    # duplicate the all fish data - will allow to put the adjusted TUNITS in
    #allfish1 <- merge(subset(allfish, as.Date(SDATE) < as.Date("1981-01-01")), stratum, by = "STRAT")
    #allfish2 <- merge(subset(allfish, as.Date(SDATE) >= as.Date("1981-01-01")), stratum2, by = "STRAT")
   # allfish <- rbind(allfish1, allfish2)

    #want a table of number of male female and unsexed for each year
    #fishlength$year<-substr(as.character(fishlength$SDATE), 1,4)
    #dat<-fishlength[,c('year', 'FSEX', 'CLEN')]
    #reshape2::dcast(dat, year~FSEX, sum)
    #write.csv(fishlength,file.path(datadir,"RVfishlength.csv"),row.names = F)
    #write.csv(allfish,file.path(datadir,"RVallfish.csv"),row.names = F)

    write.csv(fishlength,file.path(datadir,"DFOES_fishlength.csv"),row.names = F)
    write.csv(sets,file.path(datadir,"DFOES_allfish.csv"),row.names = F)
  }
  if(use.local==T){
    fishlength<-read.csv(file.path(datadir,"DFOES_fishlength.csv"))
    sets<-read.csv(file.path(datadir,"DFOES_allfish.csv"))
    #fishlength<-read.csv(file.path(datadir,"RVfishlength.csv"))
    #allfish<-read.csv(file.path(datadir,"RVallfish.csv"))
  }


  return(list(fishlength, sets))
}
