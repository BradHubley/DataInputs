#' @export
HCR<-function(RS,q=0.002,Fmsy=0.087,Bmsy=27310){

  B = RS/q

  f = calcArticulatedHCR(B,Bmsy,Fref=Fmsy)

  TAC = B*(1-exp(-f))
  #TAC = B*u


  return(TAC)

}
