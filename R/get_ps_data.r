get_ps_data<-function(datadir=getwd(),fn.oracle.username = uid, fn.oracle.password = pwd, fn.oracle.dsn = 'ptran',usepkg='roracle',force.extract=T){


  drv <- dbDriver("Oracle")
  #con <- dbConnect(drv, "WRINGEB", "Really07", dbname="ptran")
  con <- dbConnect(drv, fn.oracle.username, fn.oracle.password, dbname=fn.oracle.dsn)

  query_hold = dbSendQuery(conn = con, statement ='select * from mfd_port_samples.GPLENGTHS')
  GPLENGTHS <- fetch(query_hold)

  query_hold = dbSendQuery(conn = con, statement ='select * from mfd_port_samples.GPSAMPLES')
  GPSAMPLES <- fetch(query_hold)

  query_hold = dbSendQuery(conn = con, statement ='select * from mfd_port_samples.GPMARKETS')
  GPMARKETS <- fetch(query_hold)

  query_hold = dbSendQuery(conn = con, statement ='select * from mfd_port_samples.GPUNIQ_AREA2')
  GPUNIQ_AREA2 <- fetch(query_hold)

  save(GPLENGTHS,file.path(datadir,"PS.GPLENGTHS.RData"))
  save(GPSAMPLES,file.path(datadir,"PS.GPSAMPLES.RData"))
  save(GPMARKETS,file.path(datadir,"PS.GPMARKETS.RData"))
  save(GPUNIQ_AREA2,file.path(datadir,"PS.GPUNIQ_AREA2.RData"))

}

