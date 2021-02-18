#Nell's "get length and weight" function -> can use this to get data to us AB function on


get.lenwt.all<-function(start.year, end.year){
  start.year <- as.numeric(1970)
  end.year <- as.numeric(2013)

  #pulls all rv survey and observer data for length and weight
  #identifies outliers (out=1) that are 175%+ predicted weight and less than 25% of predicted weight

  require(ROracle)
  require(sqldf)

  ######### OBSERVER ##############
  ch1 <- ROracle::dbConnect(drv = DBI::dbDriver("Oracle"),  username = "hubleyb", password = "R4#vmxtas", dbname = "PTRAN")
  my.query <- paste("
select (TO_CHAR(t.board_date,'YYYY')) yr, to_char(t.board_date,'MM') month,  f.sexcd_id sex,substr(s.NAFAREA_ID,1,2) nafo,
f.FISH_length len, f.FISH_weight wt, g.GEARCD_ID gear
from observer.isfish f, observer.iscatches c, observer.isfishsets s, observer.ISTRIPS t, observer.ISGEARS g
where f.CATCH_ID= c.CATCH_ID and   s.FISHSET_ID = c.FISHSET_ID and    s.TRIP_ID= t.TRIP_ID and g.GEAR_ID = s.GEAR_ID
and c.SPECCD_ID = 30 and f.FISH_weight > 5 and substr(UPPER(s.NAFAREA_ID),1,2) in ('4V','4W','4X','5Z', '3N','3O','3P')
")
  query_hold = dbSendQuery(conn = ch1, statement = my.query)
  temp1 <- fetch(query_hold, errors=TRUE)
  names(temp1)<-c('year','month','sex','nafo','len','wt', 'gear')
  temp1<-temp1[temp1$year>=start.year&temp1$year<=end.year,]
  temp1$gear[!temp1$gear%in%c(51,52)]<-3  ##other
  temp1$gear[temp1$gear%in%c(51,52)]<-1   ##longline


  ########  DFO RV   ###############
  ch2 <- ROracle::dbConnect(drv = DBI::dbDriver("Oracle"),  username = "hubleyb", password = "R4#vmxtas", dbname = "PTRAN")
  my.query <- paste(" SELECT to_char(sdate,'YYYY') year, to_char(sdate,'MM') month, fsex, m.unit, flen, fwt
from groundfish.GSINF i, GROUNDFISH.GSDET d, groundfish.gsmgt m
where i.mission = d.mission and i.setno = d.setno  and i.strat = m.strat
and UPPER(m.unit) in ('4VS', '4VN', '4W','4X','5Z') and spec = 30 AND fsex IS NOT NULL AND fwt IS NOT NULL
order by year, fsex, flen")
  query_hold = dbSendQuery(conn = ch2, statement = my.query)
  temp2 <- fetch(query_hold, errors=TRUE)
  names(temp2)<-c('year','month','sex','nafo','len','wt')
  temp2<-temp2[temp2$year>=start.year&temp2$year<=end.year,]
  temp2$gear<-2  ## DFO RV TRAWL

  dat<-rbind(temp1,temp2)
  dat$out<-0
  dat<-dat[!is.na(dat$len),]
  dat<-dat[!is.na(dat$wt),]

  #identify outliers
  mod1<-nls(wt~a*len^b,data=dat,start=c(a=.001,b=3),na.action=na.omit)
  dat$upper<-predict(mod1, list(len = dat$len))*1.75
  dat$lower<-predict(mod1, list(len = dat$len))*0.25
  dat$out[dat$wt<dat$upper&dat$wt>dat$lower]<-1
  lenwt.dat.all<-dat[,c(1:8)]

  dbDisconnect(ch1)
  dbDisconnect(ch2)
  return(lenwt.dat.all)
}

