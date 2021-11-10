#' @export
BarPlotLF<-function(LF,yrs=2010:2016,LFyrs=yrs,bins=seq(0,220,5),filen='LF',rows=length(yrs),graphic='pdf',xl,rel=T,ylp=0.1,ymax,LS=81,add.sample.size=T,recline=NULL,wd=8,ht=11,bx=F,xlab="Length (cm)",ylab="Number of Halibut",toplab=NULL,ax,...){

    mids<-bins[-1]-diff(bins)/2
    if(missing(ax))ax<-rep(2,length(LF))

    if(!missing(ymax)&&length(ymax)==1)ymax<-rep(ymax,length(LF))

    if(graphic=="R") x11(width = wd, height = ht)
    if(graphic=="pdf") pdf(paste0(filen,".pdf"), width = wd, height = ht)
    if(graphic=='png') png(paste0(filen,".png"), width = wd, height = ht,units='in',pointsize=12, res=300,type='cairo')
    par(mfcol=c(rows,ceiling(length(yrs)/rows)), mar = c(0,2,0,0.5), omi = c(0.85, 0.75, 0.75, 0.5))

    if(add.sample.size)sample.size=lapply(LF,sum)

    for(i in 1:length(LF)){
    #browser()

    if(missing(xl))xlm<-c(0,length(bins))
    else if(!missing(xl))xlm<-xl

        yl2<-ifelse(missing(ymax),max(c(colSums(LF[[i]]))*1.2,1),ymax[i])

        barplot(LF[[i]],space=0,xlim=xlm,yaxt='n',xaxt='n',ylim=c(0,yl2),...)

        axis(1,1:length(bins),lab=F,tcl=-0.3)
        axis(1,lab=F,tcl=-0.6)
        if(i>1)axis(3,lab=F,tcl=0)
        if(rel==F){
            if(!is.na(ax[i]))axis(ax[i], pretty(c(0,yl2/1.3)),las=1)
            axis(2, pretty(c(0,yl2/1.3)),lab=F)
            axis(4, pretty(c(0,yl2/1.3)),lab=F)
        }
        if(rel==T){
            if(!is.na(ax[i]))axis(ax[i],seq(0,max(colSums(LF[[i]])),l=6),lab=seq(0,100,20),las=1)
            axis(2,seq(0,max(colSums(LF[[i]])),l=6),lab=F,las=1)
            axis(4,seq(0,max(colSums(LF[[i]])),l=6),lab=F,las=1)
        }
        if(i==length(LF)||i==rows){
            axis(1,at=pretty(xlm),lab=pretty(xlm)*diff(bins)[1])
        }

        if(!is.null(LS))abline(v=LS/diff(bins)[1],lwd=2,col='red')
        if(!is.null(recline))abline(v=recline,lty=2,col='red')

        mtext(as.character(LFyrs[i]), 3, -3, adj=ylp,outer = F,cex=1)
        if(add.sample.size)mtext(paste("N =",sample.size[i]), 3, -3, adj=0.9,outer = F,cex=1)
    }
    #mtext(names(LF)[i], 3, 3, outer = T, cex = 1.5)
    mtext(xlab, 1, 3, outer = T, cex = 1.5)
    if(rel==F)mtext(ylab, 2, 2, outer = T, cex = 1.5)
    if(rel==T)mtext("Relative frequency (%)", 2, 2, outer = T, cex = 1.25)
    if(!is.null(toplab))mtext(toplab, 3, 2, outer = T, cex = 1.25)
    if(bx)box()


    if(graphic%in%c("pdf","png"))dev.off()
}

