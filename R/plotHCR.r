#' @export

plotHCR <- function(  obj = list(blob),
                       yl=c(0,0.11),xl=c(0,50),
                       lw=c(2,3),clrs=c('black','grey'),lt=c(1,2),
                       vlines = c(0.5,0.8,1,1.2),
                       vcols = c("red","orange","darkgreen","darkblue"),
                       lang='en')
{

  # set up plot
  if(lang=='en'){
    options(OutDec = ".")
    labels = c( "     LRP",
                "     USR",
                "     Bmsy",
                "1.2 Bmsy")
  }
  if(lang=='fr'){
    options(OutDec = ",")
    labels = c( "     PRL",
                "     PRS",
                "     Brmd",
                "1,2 Brmd")
  }

  par(  mfrow = c(2,1),
        mar = c(.1,2,.1,2),
        oma = c(3,3,1,1) )

  plot( x = xl, y = yl, type = "n",
        las = 1,
        xlab = "", axes = FALSE,
        cex.lab = 1.5,
        cex.axis = 1.5 )

  axis(side = 2, las = 1)
  grid()
  box()
  deltaTACrule<-list()

  for(i in 1:length(obj)){


    # Get LCP and UCP from ctlList
    hcrList <- obj[[i]]$ctlList$mp$hcr

    hcrType <- hcrList$type

    survIdx <- obj[[i]]$ctlList$mp$assess$idxFleets

    if(survIdx == 6)
      survName <- "Fixed station "

    if(survIdx == 7)
      survName <- "Stratified random "

    vlines[1] <- hcrList$ctlPts[1]

    Bref  <- hcrList$inputBref_p
    Fref  <- hcrList$inputF_p

    B0    <- obj[[i]]$rp[[1]]$B0_sp
    if(!is.null(obj[[i]]$ctlList$opMod$posts))
      B0    <- mean(obj[[i]]$ctlList$opMod$posts$B0_ip)

    x <- seq(0,B0, length.out = 500 )

    LCP   <- hcrList$LCP
    UCP   <- hcrList$UCP

    if(hcrType == "conF")
    {
      y <- rep(Fref,500)

      Flabs  <- rep(Fref, length(vlines))
    }

    if(hcrType == "ramped")
    {
      LCP   <- hcrList$LCP
      UCP   <- hcrList$UCP

      y <- sapply(  X = x, FUN = calcRampedHCR,
                    LCP = LCP*Bref,
                    UCP = UCP*Bref,
                    Fref = Fref,
                    lowFmult = 0.5,
                    hiFmult = 1)

      Flabs <- sapply(  X = vlines * Bref,
                        FUN = calcRampedHCR,
                        LCP = LCP*Bref,
                        UCP = UCP*Bref,
                        Fref = Fref,
                        lowFmult = 0.5,
                        hiFmult = 1 )
    }

    if( hcrType == "articulated" )
    {
      ctlPts  <- hcrList$ctlPts
      relFs   <- hcrList$relFs

      y <- sapply(  X = x,
                    FUN = calcArticulatedHCR,
                    Bref = Bref,
                    ctlPts = ctlPts,
                    Fref = Fref,
                    relFs = relFs )

      Flabs <- sapply(  X = vlines * Bref,
                        FUN = calcArticulatedHCR,
                        Bref = Bref,
                        ctlPts = ctlPts,
                        Fref = Fref,
                        relFs = relFs )


    }

    Flabs   <- round(Flabs,3)

    deltaCtlPts   <- hcrList$deltaCtlPts
    deltaTAC      <- hcrList$deltaTAC

    deltaTACrule[[i]]  <- 100* sapply( FUN = calcArticulatedHCR,
                                       X = x,
                                       Bref = Bref,
                                       ctlPts = deltaCtlPts,
                                       Fref = 1,
                                       relFs = deltaTAC )

    legText <- paste0(labels," = ", round(Bref*vlines,1)," kt " )


    lines(x = x, y = y, lwd = lw[i], col = clrs[i], lty=lt[i])
    if(lang=='en')mtext(side = 2, text = "Fishing mortality rate (/yr)", line = 3 )
    if(lang=='fr')mtext(side = 2, text = "Taux de mortalité par pêche (/an)", line = 3 )
    abline( v = Bref*vlines,
            col = vcols,
            lty = 2, lwd = 2 )
    legend( x = "bottomright",
            bty = "n",
            legend = legText,
            col = vcols,
            lty = 2, lwd = 2)
  }


  plot( x = xl, y = c(0,100), type = "n",
        las = 1, axes = FALSE,
        cex.lab = 1.5,
        cex.axis = 1.5 )
  axis(side =1)
  axis(side =2, las= 1)
  grid()
  box()
  if(lang=='en'){
    mtext( side = 1, text= paste0(survName, "survey biomass (kt)"), line = 2 )
    mtext( side =2, text = "Maximum inter-annual change in TAC (%)", line = 3)
  }
  if(lang=='fr'){
    mtext( side = 1, text= paste0(survName, "Biomasse du relevé à stations aléatoire (en kt)"), line = 2 )
    mtext( side =2, text = "Variation annuelle maximale du TAC (en %)", line = 3)
  }
  for(i in 1:length(obj)){
    lines(x = x, y = deltaTACrule[[i]], lwd = lw[i], col = clrs[i], lty=lt[i] )
  }
  abline( v = Bref*vlines,
          col = vcols,
          lty = 2, lwd = 2 )

  #TACchange <- round(deltaTAC * (1 - exp(-Fref * c(0.4,1))) * Bref * c(LCP,1),3)
  # legend( x = "topright",
  #         bty = "n",
  #         legend = c( paste0("Expected abs delta @ LCP = ", TACchange[1]," kt"),
  #                    paste0("Expected abs delta @ Bmsy = ", TACchange[2]," kt")))

  # abline( h = highF, lty = 2, col = "grey70" )


} # END plotHCR()

