SRfromLF<-function(LF){
SR<-list()
for(i in 1:length(LF)){
  SR[[i]]<-LF[[i]][2,]/colSums(LF[[i]][1:2,])
}
return(do.call("rbind",SR))
}

