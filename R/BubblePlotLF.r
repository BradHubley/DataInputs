#' @export
BubblePlotLF<-function(LF,bins,yrs,log.trans=F,filen='',prop=F,LS=81,window=NULL,graphic='pdf',wd=11,ht=8,path=file.path(wd,'figures'),label=NULL,...){


  mids<-bins[-1]-diff(bins)/2

  if(graphic=='pdf')pdf(file.path(path,paste0('LC',filen,'.pdf')),width=wd,height=ht)
  if(graphic=='png')png(file.path(path,paste0('LC',filen,'.png')),width=wd,height=ht,units='in',res=200)
  if(graphic=='R')x11()
  par(mar=c(5, 5, 4, 2))
  for(i in 1:length(LF)){
    #LF[[i]]=t(LF[[i]])
    if(prop)LF[[i]]<-proportions(LF[[i]],1)
    z=as.vector(unlist(LF[[i]]))
    z[z==0]<-NA
    if(log.trans)z=log(z)
    #browser()
    symbols(rep(yrs,length(mids)),rep(mids,each=length(yrs)),circles=z,ylab="Length (cm)",xlab="Year",main=names(LF)[i],...)
    abline(h=LS,col='red')
    if(!is.null(window)){
      abline(h=window[1],col='grey')
      if(window[2]!=200)abline(h=window[2],col='grey')
    }
    text(max(yrs),max(mids),label,cex=2)
  }
  if(graphic!='R')dev.off()
}
