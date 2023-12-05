#' @export

prepRVdata<-function(RVdata,years,strat, bins, pal=F,raw=F){

  options(warn = -1)
  ##edit of rv data
  RVdata[[1]]$year<-lubridate::year(RVdata[[1]]$SDATE)
  RVdata[[2]]$year<-lubridate::year(RVdata[[2]]$SDATE)
  if(missing(years))years<-sort(unique(RVdata[[2]]$year))
  if(missing(strat))strat<-sort(unique(RVdata[[2]]$STRAT))

  RVdata[[1]]<-subset(RVdata[[1]],STRAT%in%strat)
  RVdata[[2]]<-subset(RVdata[[2]],STRAT%in%strat)


  N<-c()
  B<-c()
  Nse<-c()
  Bse<-c()

  survF.lst<-list()
  stratF.lst<-list()
  F.lst<-list()
  Fraw.lst<-list()
  survM.lst<-list()
  stratM.lst<-list()
  M.lst<-list()
  Mraw.lst<-list()
  surv.lst<-list()
  strat.lst<-list()
  Total.lst<-list()
  Traw.lst<-list()

  print(paste("Year", "Nstr", "N(000s)", "B(t)"))
  for (i in 1:length(years)){

    # Index of abundance
    n<-with(subset(RVdata[[2]],year==years[i]),tapply(TUNITS,STRAT,length))
   # print(n)
    Wh<-with(subset(RVdata[[2]],year==years[i]),tapply(TUNITS,STRAT,unique))
    Nh<-with(subset(RVdata[[2]],year==years[i]),tapply(TOTNO,STRAT,mean))
    NVh<-with(subset(RVdata[[2]],year==years[i]),tapply(TOTNO,STRAT,var))
    Bh<-with(subset(RVdata[[2]],year==years[i]),tapply(TOTWGT,STRAT,mean))
    BVh<-with(subset(RVdata[[2]],year==years[i]),tapply(TOTWGT,STRAT,var))
    N[i]<-sum(Nh*Wh)
    Nse[i]<-sqrt(sum(NVh/n*Wh^2))
    B[i]<-sum(Bh*Wh)
    Bse[i]<-sqrt(sum(BVh/n*Wh^2))
    print(paste(years[i],length(Wh),round(N[i]/1000),round(B[i]/1000)))

    survF.lst[[i]]<-subset(RVdata[[2]],year==years[i])
    survM.lst[[i]]<-subset(RVdata[[2]],year==years[i])
    surv.lst[[i]]<-subset(RVdata[[2]],year==years[i])
    # Length frequency
    for(j in 1:(length(bins)-1)){
      NbinhF<-with(subset(RVdata[[1]],year==years[i]&FLEN>=bins[j]&FLEN<bins[j+1]&FSEX==2),tapply(CLEN,SETNO,sum))
      NbinhF.dat<-data.frame(SETNO=as.numeric(names(NbinhF)),NbinhF)
      names(NbinhF.dat)[2]<-paste("L",bins[j+1],sep='')
      survF.lst[[i]]<-merge(survF.lst[[i]],NbinhF.dat,all=T)

      NbinhM<-with(subset(RVdata[[1]],year==years[i]&FLEN>=bins[j]&FLEN<bins[j+1]&FSEX==1),tapply(CLEN,SETNO,sum))
      NbinhM.dat<-data.frame(SETNO=as.numeric(names(NbinhM)),NbinhM)
      names(NbinhM.dat)[2]<-paste("L",bins[j+1],sep='')
      survM.lst[[i]]<-merge(survM.lst[[i]],NbinhM.dat,all=T)

      Nbinh<-with(subset(RVdata[[1]],year==years[i]&FLEN>=bins[j]&FLEN<bins[j+1]),tapply(CLEN,SETNO,sum))
      Nbinh.dat<-data.frame(SETNO=as.numeric(names(Nbinh)),Nbinh)
      names(Nbinh.dat)[2]<-paste("L",bins[j+1],sep='')
      surv.lst[[i]]<-merge(surv.lst[[i]],Nbinh.dat,all=T)
    }

    # Females
    survF.lst[[i]][is.na(survF.lst[[i]])]<-0
    stratF.lst[[i]]<-aggregate(survF.lst[[i]][c(paste("L",bins[-1],sep=''),"TUNITS")], list(survF.lst[[i]]$STRAT), mean)
    F.lst[[i]]<-colSums(sweep(stratF.lst[[i]][paste("L",bins[-1],sep='')],1,FUN='*',stratF.lst[[i]]$TUNITS))
    Fraw.lst[[i]]<-colSums(survF.lst[[i]][paste("L",bins[-1],sep='')])

    # Males
    survM.lst[[i]][is.na(survM.lst[[i]])]<-0
    stratM.lst[[i]]<-aggregate(survM.lst[[i]][c(paste("L",bins[-1],sep=''),"TUNITS")], list(survM.lst[[i]]$STRAT), mean)
    M.lst[[i]]<-colSums(sweep(stratM.lst[[i]][paste("L",bins[-1],sep='')],1,FUN='*',stratM.lst[[i]]$TUNITS))
    Mraw.lst[[i]]<-colSums(survM.lst[[i]][paste("L",bins[-1],sep='')])

    # Combined
    surv.lst[[i]][is.na(surv.lst[[i]])]<-0
    strat.lst[[i]]<-aggregate(surv.lst[[i]][c(paste("L",bins[-1],sep=''),"TUNITS")], list(surv.lst[[i]]$STRAT), mean)
    Total.lst[[i]]<-colSums(sweep(strat.lst[[i]][paste("L",bins[-1],sep='')],1,FUN='*',strat.lst[[i]]$TUNITS))
    Traw.lst[[i]]<-colSums(surv.lst[[i]][paste("L",bins[-1],sep='')])

  }
  RVcatlenF<-do.call("rbind",F.lst)
  RVcatlenM<-do.call("rbind",M.lst)
  RVcatlenC<-do.call("rbind",Total.lst)
  if(pal==TRUE){
    RVcatlenF<-proportions(RVcatlenF,1)
    RVcatlenM<-proportions(RVcatlenM,1)
    RVcatlenC<-proportions(RVcatlenC,1)
  }

  if(raw==TRUE){
    RVcatlenF<-do.call("rbind",Fraw.lst)
    RVcatlenM<-do.call("rbind",Mraw.lst)
    RVcatlenC<-do.call("rbind",Traw.lst)
  }
  row.names(RVcatlenF)<-years
  row.names(RVcatlenM)<-years
  row.names(RVcatlenC)<-years

  Index<-data.frame(year=years,N=N,Nse=Nse,B=B,Bse=Bse)

  tunits<-group_by(RVdata[[2]],year,STRAT)%>%summarise(mean=mean(TUNITS))%>%group_by(year)%>%summarise(tunits=sum(mean))
  Index<-merge(Index,tunits)
  Index$NPT<-Index$N/Index$tunits

  list(Index=Index,NSRV_males=RVcatlenM,NSRV_females=RVcatlenF,NSRV_combined=RVcatlenC)
}
