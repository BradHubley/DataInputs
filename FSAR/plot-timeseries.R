#' Generate 4-panel figure using simulated fisheries data in ggplot
#'
#' @param timeseries the timeseries to plot
#' @param language French or English
#'
#' @return A graphics device with the four-panel of figures
#' @export
#'
#' @examples
#' fsar_plot_ggplot(sim_fsar_data(format="wide"))
#' @importFrom ggplot2 ggplot geom_line aes scale_y_continuous labs geom_ribbon
#'   expansion
fsar_plot_ggplot <- function(df, language = c("English","French")) {

  language <- match.arg(language)

  # Catch
  g1 <- ggplot(data = df, aes(x = year)) +
    geom_line(aes(y = `Catch-MT`)) +
    geom_line(aes(y = `TAC-MT`), linetype = "dashed", colour = "grey45") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
    labs(x = "Year", y = "Catch (t)") +
    theme_csas()

  # SSB = Spawning stock biomass
  g2 <- ggplot(data = df, aes(x = year)) +
    geom_line(aes(y = `SSB-MT`)) +
    geom_ribbon(aes(ymin = `SSBlow-MT`, ymax = `SSBhigh-MT`), alpha = 0.3) +
    geom_line(aes(y = `SSBlrp-MT`), linetype = "dashed", colour = "red") +
    geom_line(aes(y = `SSBusr-MT`), linetype = "dashed", colour = "grey45") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
    labs(x = "Year", y = "SSB (t)") +
    theme_csas()

  # Instantaneous fishing mortality
  g3 <- ggplot(data = df, aes(x = year)) +
    geom_line(aes(y = `F-1/yr`)) +
    geom_ribbon(aes(ymin = `Flow-1/yr`, ymax = `Fhigh-1/yr`), alpha = 0.3) +
    geom_line(aes(y = `Flim-1/yr`), linetype = "dashed", colour = "grey45") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
    labs(x = "Year", y = "Fishing mortality (yr<sup>-1</sup>)") +
    theme_csas() +
    theme(axis.title.y = ggtext::element_markdown())

  g4 <- ggplot(data = df, aes(x = year)) +
    geom_line(aes(y = `R-E06`)) +
    geom_ribbon(aes(ymin = `Rlow-E06`, ymax = `Rhigh-E06`), alpha = 0.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02)), limits = c(0, NA)) +
    labs(x = "Year", y = "Recruitment (10<sup>6</sup>)") +
    theme_csas() +
    theme(axis.title.y = ggtext::element_markdown())

  cowplot::plot_grid(g1, g2, g3, g4, ncol = 2, labels = "AUTO", align = "hv")

}


#' Generate 4-panel figure using simulated fisheries data in base R
#'
#' @param timeseries the timeseries to plot
#' @param language French or English
#'
#' @return A graphics device with the four-panel of figures
#' @export
#'
#' @examples
#' fsar_plot_base(sim_fsar_data(format="long"))
#'
#' @importFrom grDevices grey
#' @importFrom graphics axis box layout legend lines mtext par
#' @importFrom stats arima.sim rnorm
fsar_plot_base <- function(in.df, Assessment.Year=2024, language = c("English","French")) {

  language <- match.arg(language)
  colrs<-c('#377eb8','#d53e4f','#7fbc41')

  mm <- matrix(c(rep(0, 5), 0, 1, 0, 2, 0, rep(0, 5), 0, 3, 0, 4, 0, rep(0, 5)), nc = 5, byrow = TRUE)
  ll <- layout(mm, widths = c(0.06, 0.43, 0.06, 0.43, 0.02), heights = c(c(0.02, 0.45, 0.04, 0.45, 0.04))) # layout.show(ll)
  par(mar = c(2, 2, 0, 0))

  # top-left panel - Catch and TAC
  idx <- which(in.df$panel.category == "Catch")
  yl <- c(0, max(in.df[idx, "ts.value"],na.rm=T) * 1.2)
  tl.df <- in.df[idx, ]

  if (language == "English") {
    x.lab <- "Year"
    y.lab <- "Catch (T)"
    legend.text <- c("Canada","Foreign", "TAC")
  }
  if (language == "French") {
    x.lab <- "Année"
    y.lab <- "Capture"
    legend.text <- c("Capture-tonne", "TAC-tonne")
  }

  y1<-min(tl.df$year)
  plm<-pivot_wider(tl.df,names_from = year, values_from = ts.value)
  plm<-as.matrix(plm[c(1,2,4),-(1:2)])
  plm[is.na(plm)]<-0


  barplot(plm,ylim=yl,xlim=c(0,(Assessment.Year-y1+4)*1.2), axes = FALSE, col=c(colrs[1],colrs[2],colrs[1]),border=NA,density=c(NA,NA,40),axisnames=F)
  #axis(1,at=seq(0.7,(Assessment.Year-1959)*1.2,1.2),lab=F,tck=-0.01) ##change to 77 in sequence to increase axis length (ticks)
  axis(1,at=seq(0.7,(Assessment.Year-y1+1)*1.2,12),lab=seq(y1,Assessment.Year,10))##75 remains (no change) since 2023 not a multiple of 10
  axis(2, at = seq(0, yl[2], 1000), las = 1,
       labels = prettyNum(seq(0, yl[2],1000),
                          big.mark = ',',
                          scientific = FALSE))
  lines(seq(0.7,(Assessment.Year-y1+1)*1.2,1.2),in.df$ts.value[in.df$ts.name=="TAC"],col=colrs[3],lwd=3) ##change to 78 to accommodate additional year
  legend("topright","(A)", bty = "n", cex=1.25)
  legend('topleft',
         legend.text,
         pch=c(15,15,NA),
         col=c(colrs[1],colrs[2],colrs[3]),
         lwd=c(NA,NA,3),
         box.lty=0,
         bg=NA,
         cex=0.8)

  mtext(side = 1, x.lab, line = 2, cex = 0.75)
  mtext(side = 2, y.lab, line = 3.5, cex = 0.75)
  box()


  # top-right panel
  idx <- which(in.df$panel.category == "Biomass")
  yl <- c(0, max(in.df[idx, "ts.value"]) * 1.2)
  tr.df <- in.df[idx, ]
  #xl <- range(tr.df$year)
  xl <- c(1970,Assessment.Year+3)

  if (language == "English") {
    y.lab <- "Biomass (KT)"
    legend.text <- c("Halibut Survey (modelled)","95% confidence","Halibut Survey (projection)","90% projection envelope", "Halibut Survey (index)", "3-year mean of index","USR", "LRP")
  }
  if (language == "French") {
    y.lab <- "Biomasse KT"
    legend.text <- c("BSR-tonne", "confiance à 95%", "NRS-tonne", "NRL-tonne")
  }

  plot(ts.value ~ year, data = tr.df, type = "n", axes = FALSE, xlab = "", ylab = "", ylim = yl,xlim=xl)

  ## Vulnerable Biomass
  ## lower and upper
  yrs<-tr.df$year[tr.df$ts.name == "HSpredlow"]
  cil<-tr.df$ts.value[tr.df$ts.name == "HSpredlow"]
  cih<-tr.df$ts.value[tr.df$ts.name == "HSpredhigh"]

  polygon(  x = c(yrs,rev(yrs)),y = c(cil,rev(cih)), col = 'grey50', border = NA )
  #lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSpredlow"), ], type = "l", lty = 2)
  #lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSpredhigh"), ], type = "l", lty = 2)
  lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSpred"), ], type = "l", lwd = 1,col='grey30',lty=1)

  ## Vulnerable Biomass projection
  ## lower and upper
  yrs<-tr.df$year[tr.df$ts.name == "HSprojlow"]
  cil<-tr.df$ts.value[tr.df$ts.name == "HSprojlow"]
  cih<-tr.df$ts.value[tr.df$ts.name == "HSprojhigh"]

  polygon(  x = c(yrs,rev(yrs)),y = c(cil,rev(cih)), col ='grey80', border = NA )
  #lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSpredlow"), ], type = "l", lty = 2)
  #lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSpredhigh"), ], type = "l", lty = 2)
  lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSproj"), ], type = "l", lwd = 1,col='grey50',lty=2)

  ## SurveyBiomass
  lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSobs"), ], type = "p",pch=16)
  lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSobs_3yrm"), ], type = "l", lwd = 3,col='blue')


  ## LRP and USR
  abline(h=10.9, lty = 3, lwd = 2, col = "red")
  abline(h=21.8, lty = 3, lwd = 2, col = "forestgreen")
  #lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "SSBlrp-MT"), ], type = "l")
  #lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "SSBusr-MT"), ], type = "l"

  legend("topright","(B)", bty = "n", cex=1.25)
  legend("topleft",
         legend.text,
         lty = c(1, NA, 2, NA, NA, 1, 3, 3),
         lwd = c(1, NA, 1, NA, NA, 3, 2, 2),
         pch = c(NA,15,NA,15,16,NA,NA,NA),
         col = c('grey30','grey50','grey50','grey80',"black", "blue", "forestgreen", "red"),
         box.lty=0,
         bg=NA,
         cex=0.8
  )

  axis(side = 1, padj = -0.5)
  axis(side = 2, las = 1, hadj = 0.9)
  mtext(side = 1, x.lab, line = 2, cex = 0.75)
  mtext(side = 2, y.lab, line = 3, cex = 0.75)
  box()

  ## bottom-left panel
  idx <- which(in.df$panel.category == "Fishing")
  yl <- c(0, max(in.df[idx, "ts.value"]) * 1.2)
  bl.df <- in.df[idx, ]

  if (language == "English") {
    y.lab <- "Mortality (1/yr)"
    legend.text <- c("Fishing", "95% confidence", "Natural", "95% confidence", "1.2FMSY", "FMSY", "0.8FMSY")
  }
  if (language == "French") {
    y.lab <- "Mortalité"
    legend.text <- c("F-1/yr", "confiance à 95%", "RP-1/yr", "M-1/yr")
  }

  plot(ts.value ~ year, data = bl.df, type = "n", axes = FALSE, xlab = "", ylab = "", ylim = yl,xlim=xl)

  ## Fishing
  yrs<-bl.df$year[bl.df$ts.name == "Utlow"]
  cil<-bl.df$ts.value[bl.df$ts.name == "Utlow"]
  cih<-bl.df$ts.value[bl.df$ts.name == "Uthigh"]

  polygon(  x = c(yrs,rev(yrs)),y = c(cil,rev(cih)), col = 'grey', border = NA )
  lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "Ut"), ], type = "l", lwd = 1,col='grey40',lty=1)

  ## natural mortality
  yrs<-bl.df$year[bl.df$ts.name == "Mtlow"]
  cil<-bl.df$ts.value[bl.df$ts.name == "Mtlow"]
  cih<-bl.df$ts.value[bl.df$ts.name == "Mthigh"]

  polygon(  x = c(yrs,rev(yrs)),y = c(cil,rev(cih)), col = rgb(1,0,0,0.2), border = NA )
  lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "Mt"), ], type = "l", lwd = 1,col='red',lty=2)

  #removal reference
  abline(h=0.0877*c(1.2,1,0.8), lty = 3, lwd = 2,col=c("blue", "forestgreen", "orange"))

  #lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "M-1/yr"), ], lty = 1, lwd = 2, col = grey(0.5))


  legend("topright","(C)", bty = "n", cex=1.25)
  legend("topleft",
         legend.text,
         lty = c(1, NA, 2, NA, 3, 3, 3),
         lwd = c(1, NA, 1, NA, 2, 2, 2),
         pch = c(NA, 15, NA, 15, NA, NA, NA),
         col = c('grey40','grey','red',rgb(1,0,0,0.2), "blue", "forestgreen", "orange"),
         box.lty = 0,
         bg=NA,
         cex=0.8
  )

  axis(side = 1, padj = -0.5)
  axis(side = 2, las = 1, hadj = 0.9)
  # axis(side = 4, las = 1, hadj = 0.9)
  mtext(side = 1, x.lab, line = 2, cex = 0.75)
  mtext(side = 2, y.lab, line = 3, cex = 0.75)

  box()


  ## bottom-right panel
  idx <- which(in.df$panel.category == "Recruitment")
  yl <- c(0, max(in.df[idx, "ts.value"]) * 1.2)
  br.df <- in.df[idx, ]

  if (language == "English") {
    y.lab <- "Abundance (millions)"
    legend.text <- c("RV Survey (modelled)", "95% confidence", "RV Survey (Index)")
  }
  if (language == "French") {
    y.lab <- "Recrutement"
    legend.text <- c("R-E06", "confiance à 95%")
  }
  plot(ts.value ~ year, data = br.df, type = "n", axes = FALSE, xlab = "", ylab = "", ylim = yl,xlim=xl)
  ## Vulnerable Abundance
  ## lower and upper
  yrs<-br.df$year[br.df$ts.name == "RVpredlow"]
  cil<-br.df$ts.value[br.df$ts.name == "RVpredlow"]
  cih<-br.df$ts.value[br.df$ts.name == "RVpredhigh"]

  polygon(  x = c(yrs,rev(yrs)),y = c(cil,rev(cih)), col = 'grey', border = NA )
  lines(ts.value ~ year, data = br.df[which(br.df$ts.name == "RVpred"), ], type = "l", lwd = 1,col='grey40',lty=1)

  ## Survey Abundance
  lines(ts.value ~ year, data = br.df[which(br.df$ts.name == "RVobs"), ], type = "p",pch=16)

  legend("topright","(D)", bty = "n", cex=1.25)
  legend("topleft",
         legend.text,
         lty = c(1, NA, NA),
         lwd = c(1, NA, NA),
         pch = c(NA,15,16),
         col = c('grey40','grey',"black"),
         box.lty = 0,
         bg=NA,
         cex=0.8
  )


  axis(side = 1, padj = -0.5)
  mtext(side = 1, x.lab, line = 2, cex = 0.75)
  axis(side = 2, las = 1, hadj = 0.9)
  mtext(side = 2, y.lab, line = 3, cex = 0.75)
  box()
} # end function definition

