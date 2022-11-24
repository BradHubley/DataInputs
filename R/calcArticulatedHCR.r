# calcArticulatedHCR() from MS3-HAL
# Generalisation of DFO PA compliant ramped HCR, generates
# a piecewise linear function between an arbitrary number
# of control points, with relative F values at each.
# Arguments:  B       = current biomass estimate
#             Bref    = Reference biomass scaled by ctl points
#             ctlPts  = Vector of multiples of Bref for HCR 'corners'
#             Fref    = Maximum removal reference
#             relFs   = Proportions of Fref applied at corners; end
#                       points extend to 0/Inf
#' @export
calcArticulatedHCR <- function( B,
                                Bref,
                                ctlPts = c(0.4,0.8,1.2),
                                Fref,
                                relFs = c(0.575,0.8,1.2) )
{
  ctlPts <- ctlPts * Bref
  nPts   <- length(ctlPts)

  # Check endpoints first
  if(B <= ctlPts[1])
  {
    F <- Fref * relFs[1]
    return(F)
  }

  if(B >= ctlPts[nPts])
  {
    F <- Fref * relFs[nPts]
    return(F)
  }

  for( c in 1:(nPts-1))
  {
    if(B >= ctlPts[c] & B < ctlPts[c+1])
    {
      relF <- relFs[c] + (B - ctlPts[c])/(ctlPts[c+1] - ctlPts[c]) * (relFs[c+1] - relFs[c])
      F <- relF * Fref
      return(F)
    }
  }

  message("Something went wrong in Articulated HCR!!\n")
  browser()
} # END calcArticulatedHCR()
