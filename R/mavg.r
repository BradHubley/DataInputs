mavg <- function(index,n=3,geometric=F){

  out <- c()
  for(i in n:length(index)){
    out[i] <- mean(index[i:(i-n+1)])
    if(geometric==T) out[i] <- exp(mean(log(index[i:(i-n+1)])))
  }
  return(out)
}
