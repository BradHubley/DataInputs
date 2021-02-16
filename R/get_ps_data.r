get_ps_data<-function(data.dir=getwd(),fn.oracle.username = uid, fn.oracle.password = pwd, fn.oracle.dsn = 'ptran',usepkg='roracle',force.extract=F){

  library(ROracle)
  if(force.extract==T){
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

    save(GPLENGTHS,file=file.path(data.dir,"PS.GPLENGTHS.RData"))
    save(GPSAMPLES,file=file.path(data.dir,"PS.GPSAMPLES.RData"))
    save(GPMARKETS,file=file.path(data.dir,"PS.GPMARKETS.RData"))
    save(GPUNIQ_AREA2,file=file.path(data.dir,"PS.GPUNIQ_AREA2.RData"))

  }
  else{
    load(file.path(data.dir,"PS.GPLENGTHS.RData"))
    load(file.path(data.dir,"PS.GPSAMPLES.RData"))
    load(file.path(data.dir,"PS.GPMARKETS.RData"))
    load(file.path(data.dir,"PS.GPUNIQ_AREA2.RData"))
  }

  samples <- left_join(GPSAMPLES,data.frame(AREA=GPUNIQ_AREA2$AREACODE,NAFO=GPUNIQ_AREA2$DESCRIPTION2))

  samples <- left_join(subset(samples,SPECIES=="030",c("SAMPLE","DATELANDED","SAMPLED","CFV","TRIP_NUMBER","NAFO")),GPMARKETS)

  portSamples <- left_join(samples,GPLENGTHS)

  #assign("GPLENGTHS",GPLENGTHS,pos=1)
  #assign("GPSAMPLES",GPSAMPLES,pos=1)
  #assign("GPMARKETS",GPMARKETS,pos=1)
  #assign("GPUNIQ_AREA2",GPUNIQ_AREA2,pos=1)

  return(portSamples)

}

