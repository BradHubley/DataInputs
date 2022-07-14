#' @export
RVindex <- function(uid, pwd, use.local=T,plot=T,AY=lubridate::year(Sys.time())-1,datadir="C:/Users/hubleyb/Documents/Halibut/data"){

  if(use.local==F){
    drv.fn <- DBI::dbDriver("Oracle")

    # Make connection to Database PTRAN
    con <- ROracle::dbConnect(drv = drv.fn, username = uid, password = pwd, dbname = "ptran")

    # Get INF data (information on fishing conditions) - use statement provided by Nell
    get_strat <- ROracle::dbSendQuery(conn = con, statement = "SELECT MIN(CREATE_date) CREATE_date, i.series, i.year,
                    ROUND(SUM(s.units)) units, ROUND(SUM(str_totno)) abundance,
                   ROUND(SQRT(SUM(s.units*(s.units-i.ntows)*var_totno/i.ntows))) abundance_se,
           ROUND(SUM(str_totno)/SUM(s.units),2) num_per_tow,
                   ROUND(SQRT((SUM(s.units*(s.units-i.ntows)*var_totno/i.ntows))
                     /(POWER(SUM(s.units),2))),2) num_per_tow_se,
           ROUND(SUM(str_totwgt)) biomass,
                   ROUND(SQRT(SUM(s.units*(s.units-i.ntows)*var_totwgt/i.ntows)),2) biomass_se,
                   ROUND(SUM(str_totwgt)/SUM(s.units),2) wgt_per_tow,
                   ROUND(SQRT((SUM(s.units*(s.units-i.ntows)*var_totwgt/i.ntows))
                     /(POWER(SUM(s.units),2))),2) wgt_per_tow_se
    FROM  mflib.gs_str_nw_view c,
           nwags.gssinf_mv i,
           (select strat, area, ROUND(floor(area/(1.75*(41/6080.2))+.5)) units
              from groundfish.gsstratum
             where strat IN ('441','443','454','463','472','483','405','440','446','451','460','484','409','447','453','462','465','470','475','477','491','410','445','450','452','459','467','468','471','485','494','495','404','442','469','474','480','476','478','481','482','401','411','444','449','455','458','461','464','490','403','406','407','408','448','456','457','466','473','492','493')) s
    WHERE i.strat=s.strat
      AND i.series=c.series(+)
      AND i.year=c.year(+)
      AND i.strat=c.strat(+)
      AND c.spec(+)=30
     AND i.series=upper('SUMMER')
      AND i.year between 1970 and 2020
      AND i.ntows >1
    GROUP BY i.series, i.year
    ORDER BY i.year DESC")

    #get_strat <- ROracle::dbSendQuery(conn = con, statement = "SELECT * FROM  mflib.gs_str_nw_view ")
    #get_strat <- ROracle::dbSendQuery(conn = con, statement = "SELECT * FROM  nwags.gssinf_mv ")
    strat <- ROracle::fetch(get_strat)
    write.csv(strat,file.path(datadir,"RVstrat.csv"),row.names = F)
  }
  if(use.local==T){
    strat<-read.csv(file.path(datadir,"RVstrat.csv"))
  }
  if(plot==T){
    strat$NUM_PER_TOW[strat$YEAR==2018] = NA
    fig<-ggplot(strat, aes(x = YEAR, y = NUM_PER_TOW)) + geom_point() +
      geom_line() +
      geom_hline(yintercept = mean(strat[strat$YEAR < AY, ]$NUM_PER_TOW,na.rm=T)) +
      geom_errorbar(aes(ymin = NUM_PER_TOW - NUM_PER_TOW_SE*1.96, ymax = NUM_PER_TOW + NUM_PER_TOW_SE*1.96)) +
      ylab("Mean numbers per tow") +
      theme(panel.background = element_rect(colour = "black", fill = NA))
    print(mean(strat[strat$YEAR < AY, ]$NUM_PER_TOW,na.rm=T))
    return(fig)

  }
  return(strat)


}


