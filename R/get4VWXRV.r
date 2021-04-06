get.4VWXRV.data<-function(uid, pwd){
  # Set/load database
  drv.fn <- DBI::dbDriver("Oracle")
  # Make connection to Database PTRAN
  con <- ROracle::dbConnect(drv = drv.fn, username = uid, password = pwd, dbname = "ptran")
  # Get fish length data - use statement provided by Nell
  fishlength_get <- ROracle::dbSendQuery(conn = con, statement = "select i.mission,i.setno,(dmin+dmax)/2 depth, strat, sdate, dist,flen,fwt,clen, fsex from groundfish.gsinf i, groundfish.gsdet c where i.mission=c.mission and i.setno=c.setno and spec=30 and to_char(sdate,'mm') in ('06','07','08') and strat between '440' and '495'  and type=1")
  # fetch the data
  fishlength <- ROracle::fetch(fishlength_get)
  # get the required set information - requires three queries
  # Get tows with halibut - only summer survey (months 6, 7, 8)
  halibut_get <- ROracle::dbSendQuery(conn = con, statement = "select i.mission,i.setno,(dmin+dmax)/2 depth, strat, sdate, dist,totno,totwgt from groundfish.gsinf i, groundfish.gscat c where i.mission=c.mission and i.setno=c.setno and spec=30 and to_char(sdate,'mm') in ('06','07','08') and strat between '440' and '495'  and type=1")
  halibut <- ROracle::fetch(halibut_get)
  # get tows with no halibut - only summer survey (months 6, 7, 8)
  no_halibut_get <- ROracle::dbSendQuery(conn = con, statement = "select i.mission,i.setno,(dmin+dmax)/2 depth, strat, sdate,dist, 0 totno, 0 totwgt from groundfish.gsinf i where to_char(sdate,'mm') in ('06','07','08') and strat between '440' and '495' and type=1")
  no_halibut <- ROracle::fetch(no_halibut_get)
  # Get stratum data
  stratum_get <- ROracle::dbSendQuery(conn = con, statement = "select strat,area from groundfish.gsstratum")
  stratum <- ROracle::fetch(stratum_get)
  #calculate the total weight and total numbers
  halibut$TOTWGT <- halibut$TOTWGT*1.75 / halibut$DIST
  halibut$TOTNO <- halibut$TOTNO*1.75 / halibut$DIST
  ## Merge the tows with and without halibut
  allfish <- rbind(halibut, no_halibut)
  allfish <- allfish[!duplicated(allfish[, c("MISSION", "SETNO")]),]
  # convert depth to metres
  allfish$DEPTH <- allfish$DEPTH*1.8288
  # duplicate the stratum data frame - seems to have to do with how areas were measured before and after 1981
  stratum2 <- stratum
  stratum$TUNITS <- stratum$AREA / ((35/6080.2)*1.75)
  stratum2$TUNITS <- stratum2$AREA / ((41/6080.2)*1.75)
  # duplicate the all fish data - will allow to put the adjusted TUNITS in
  allfish1 <- merge(subset(allfish, as.Date(SDATE) < as.Date("1981-01-01")), stratum, by = "STRAT")
  allfish2 <- merge(subset(allfish, as.Date(SDATE) >= as.Date("1981-01-01")), stratum2, by = "STRAT")
  allfish <- rbind(allfish1, allfish2)
  #want a table of number of male female and unsexed for each year
  fishlength$year<-substr(as.character(fishlength$SDATE), 1,4)
  dat<-fishlength[,c('year', 'FSEX', 'CLEN')]
  reshape2::dcast(dat, year~FSEX, sum)
  return(list(fishlength, allfish))
}
