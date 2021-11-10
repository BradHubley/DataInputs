get.3NOP.rv.dat.2021<-function(start.year, end.year){
  require(RODBC)
  require(sqldf)
  require(reshape2)

  NLnew<-read.csv("R:/Science/Population Ecology Division/Shared/Atlantic Halibut/Assessments & Updates/2021 Framework/Data/Newfoundland Data/Newfoundland RV Data - Received 2021/strat2_abundance_summary.csv")
  datNL<-read.csv("R:/Science/Population Ecology Division/Shared/Atlantic Halibut/Assessments & Updates/2014 Framework/Nell code/hal.dat.2013.v1/NLHalibut.csv", header = TRUE)
  dim(datNL)
  #[1] 84366    23
  names(datNL)
  datNL<-datNL[datNL$SPEC==30&as.character(datNL$NAFOZONE)%in%c('3N', '3O', '3P'),]
  datNL<-datNL[as.character(datNL$SEASON1)=='Spring'&datNL$Month!=8,]
  datNL<-datNL[datNL$GEAR%in%c(1,2),]
  dim(datNL)
  summary(datNL)
  sort(unique(datNL$NAFOZONE))
  with(datNL, table(STDNO))
  ##mostly zeros, want to get expanded num

  ##remove any strata in a year with less than 2 tows
  datNL$yr_strat<-paste(datNL$Year, datNL$STRATUM)
  x<-with(datNL, table(yr_strat))
  y<-x[x<2]
  rm_strat<-names(y)
  datNL<-datNL[!datNL$yr_strat%in%rm_strat,]

  ##
  datNL<-datNL[datNL$Year>=start.year,]
  stdsetarea<-c(0.013331, 0.007274)  ##from Marianno, [1] is engel 1983-1995, [2] is campelen 1996-2013
  ##but we do not have the same for the yankee

  datNL$num[datNL$Year<=1995]<-datNL$STDNO[datNL$Year<=1995]/stdsetarea[1]
  datNL$num[datNL$Year>1995]<-datNL$STDNO[datNL$Year>1995]/stdsetarea[2]

  datNL2<-sqldf("SELECT Year, STRATUM, AVG(AreaNMsq) area, AVG(num) mean_no, Variance(num) var_no FROM datNL GROUP BY Year, STRATUM")
  datNL3<-sqldf("SELECT Year, SUM(mean_no) tot_no, SUM(area) tot_area FROM datNL2 GROUP BY Year")

  with(datNL3, plot(Year, tot_no/tot_area))

  datNL4<-merge(datNL2, datNL3, by.y='Year', by.x='Year', all.x=T)
  names(datNL4)
  datNL4$w_mean<-datNL4$mean_no*datNL4$area/datNL4$tot_area
  datNL4$w_var<-datNL4$var_no*(datNL4$area/datNL4$tot_area)^2

  datNL5<-sqldf("SELECT Year, SUM(w_mean) std_mean, sum(w_var) sum_var FROM datNL4 GROUP BY Year")

  # RV survey
  rv.datNL<-datNL5
  names(rv.datNL)<-c('year','smean','smean.var')
  rv.datNL$smean[rv.datNL$year<=1995]<-rv.datNL$smean[rv.datNL$year<=1995]*stdsetarea[1]
  rv.datNL$smean[rv.datNL$year>1995]<-rv.datNL$smean[rv.datNL$year>1995]*stdsetarea[2]
  rv.datNL$smean.se[rv.datNL$year<=1995]<-sqrt(rv.datNL$smean.var[rv.datNL$year<=1995])*stdsetarea[1]
  rv.datNL$smean.se[rv.datNL$year>1995]<-sqrt(rv.datNL$smean.var[rv.datNL$year>1995])*stdsetarea[2]

  #for previous assessment data
  # lower<-rv.datNL$smean-rv.datNL$smean.se*2
  # upper<-rv.datNL$smean+rv.datNL$smean.se*2
  # years<-1971:1995
  ylim1<-c(0,0.4)
  #
  # #for new data to 2019
  # new_lower <- NLstrat_abundance_summary$mean.lcl
  # new_upper <- NLstrat_abundance_summary$mean.ucl
  # new_years <- 1996:2019

  #with updated data to 2019
  png("NL_survey.png", width = 9.5, height = 6,units='in',pointsize=12, res=300,type='cairo')
  par(mfrow=c(1,1),omi=c(0.5,0.5,0,0), mar=c(2,2,2,.5), mgp=c(2,1,0), las=1)
  plot(1971:1982,rv.datNL[1:length(1971:1982),2],type='l',lty=1,xlim=c(1971,2019),ylim=ylim1,xlab='',ylab='',lwd=3)
  lines(1983:1995,rv.datNL[length(1971:1983):length(1971:1995),2],lwd=3)
  lines(1996:2019, NLnew[1:length(1996:2019), 7], lwd=3)

  #arrows(years,upper,years,lower,length=0.05,angle=90,code=3)  # change length to 0.05 if want horizontal lines on error bar and code=3
  #arrows(new_years, new_upper, new_years, new_lower, length = 0.05, angle = 90, code = 3)
  mtext("Mean numbers per tow",side=2,line=2.5,cex=1.1,las=0)
  mtext("3NOPs Spring RV survey",side=3,line=.5,cex=1.2,las=0,adj=0)
  segments(1971, mean(rv.datNL[1:length(1971:1982),2]), 1982, mean(rv.datNL[1:length(1971:1982),2]), col="gray60", lwd=2)
  segments(1983, mean(rv.datNL[length(1971:1982):length(1971:1995),2]), 1995, mean(rv.datNL[length(1971:1982):length(1971:1995),2]), col="gray60", lwd=2)
  segments(1996, mean(NLnew[1:length(1996:2019), 7]), 2019, mean(NLnew[1:length(1996:2019),7]), col="gray60", lwd=2)
  text(1976,0.38, "Yankee")
  text(1989,0.38, "Engel")
  text(2004,0.38, "Campelen")
  dev.off()

}


