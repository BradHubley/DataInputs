#' @export
BubblePannelPlotLF<-function(LF,bins,yrs,trans='sqrt',filen='',prop=F,LS=81,window=NULL,graphic='pdf',nc=2, wd=11,ht=8,path=file.path(wd,'figures'),lang='en',label,...){

  xl<-range(yrs)
  mids<-bins[-1]-diff(bins)/2
  if(missing(label))label<-names(LF)
  bottom<-ceiling(length(LF)/nc)

  if(graphic=='pdf')pdf(file.path(path,paste0('LC',filen,'.pdf')),width=wd,height=ht)
  if(graphic=='R')x11()
  if(graphic=='png')png(file.path(path,paste0('LC',filen,'.png')),width=wd,height=ht,units='in',res=400)
  par(mfcol=c(bottom,nc), mar = c(0,0,0,0), omi = c(0.5, 0.7, 0.5, 0.5),las=1)

  for(i in 1:length(LF)){
    #LF[[i]]=t(LF[[i]])
    yrs<-as.numeric(rownames(combinedLengths[[i]]))
    if(prop)LF[[i]]<-proportions(LF[[i]],1)
    z=as.vector(unlist(LF[[i]]))
    z[z==0]<-NA
    if(trans=='log')z=log(z)
    if(trans=='sqrt') z=sqrt(z/pi)
    #browser()
    symbols(rep(yrs,length(mids)),rep(mids,each=length(yrs)),circles=z,xlim=xl,xaxt='n',yaxt='n',...)
    abline(h=LS,col='red')
    axis(1,lab=F)
    if(i==bottom||i==length(LF))axis(1)
    if(i<=bottom)axis(2)
    if(i>bottom)axis(4)



    text(xl[1],max(mids),label[i],cex=1,pos=4)
  }
  if(lang=='fr') mtext("Longueur (en cm)",2,3,outer=T,las=0)
  else mtext("Length (cm)",2,3,outer=T,las=0)
  if(graphic!='R')dev.off()
}
