rearrangeLFlist<-function(input){
  output<-list()
  y=nrow(input[[1]])
  x=length(input)
  z=ncol(input[[1]])
  for(i in 1:y){
    output[[i]]<-matrix(nrow=x,ncol=z)
    for(j in 1:x){
      output[[i]][j,]<-input[[j]][i,]
    }
  }
  return(output)
}
