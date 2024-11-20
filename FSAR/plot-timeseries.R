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
fsar_plot_base <- function(in.df, language = c("English","French")) {

  language <- match.arg(language)
  colrs<-c('#377eb8','#d53e4f','#7fbc41')

  mm <- matrix(c(rep(0, 5), 0, 1, 0, 2, 0, rep(0, 5), 0, 3, 0, 4, 0, rep(0, 5)), nc = 5, byrow = TRUE)
  ll <- layout(mm, widths = c(0.06, 0.43, 0.06, 0.43, 0.02), heights = c(c(0.02, 0.45, 0.04, 0.45, 0.04))) # layout.show(ll)
  par(mar = c(2, 2, 0, 0))

  # top-left panel - Catch and TAC
  idx <- which(in.df$panel.category == "Catch")
  yl <- c(0, max(in.df[idx, "ts.value"]) * 1.4)
  tl.df <- in.df[idx, ]

  if (language == "English") {
    x.lab <- "Year"
    y.lab <- "Catch"
    legend.text <- c("Canada-Catch-MT","Foreign-Catch-MT", "TAC-MT")
  }
  if (language == "French") {
    x.lab <- "Année"
    y.lab <- "Capture"
    legend.text <- c("Capture-tonne", "TAC-tonne")
  }

  plm<- filter(in.df,panel.category=="Catch") |>
    pivot_wider(names_from = year, values_from = ts.value)
  plm<-as.matrix(plm[c(1,2,4),-(1:2)])
  plm[is.na(plm)]<-0
  yl <- c(0,max(colSums(plm))*1.1)


  barplot(plm,ylim=yl, axes = FALSE, col=c(colrs[1],colrs[2],colrs[1]),border=NA,density=c(NA,NA,40),axisnames=F)
  axis(1,at=seq(0.7,(Assessment.Year-1959)*1.2,1.2),lab=F,tck=-0.01) ##change to 77 in sequence to increase axis length (ticks)
  axis(1,at=seq(0.7,(Assessment.Year-1959)*1.2,12),lab=seq(1960,Assessment.Year,10))##75 remains (no change) since 2023 not a multiple of 10
  axis(2, at = seq(0, yl[2], 1000), las = 1,
       labels = prettyNum(seq(0, yl[2],1000),
                          big.mark = ',',
                          scientific = FALSE))
  lines(seq(0.7,(Assessment.Year-1959)*1.2,1.2),in.df$ts.value[in.df$ts.name=="TAC"],col=colrs[3],lwd=4) ##change to 78 to accommodate additional year
  legend('topright',legend.text,fill=c(colrs[1],colrs[2],NA),col=c(NA,NA,colrs[3]),lwd=c(NA,NA,4),border=NA,inset=c(0.2,0.1),bg='white',box.lty=0)

  mtext(side = 1, x.lab, line = 2.5, cex = 0.75)
  mtext(side = 2, y.lab, line = 3.5, cex = 0.75)
  box()


  # top-right panel
  idx <- which(in.df$panel.category == "Biomass")
  yl <- c(0, max(in.df[idx, "ts.value"]) * 1.2)
  tr.df <- in.df[idx, ]

  if (language == "English") {
    y.lab <- "Biomass KT"
    legend.text <- c("Survey Biomass (modelled)","95% confidence", "Survey Biomass (index)", "3-year mean of index","USR", "LRP")
  }
  if (language == "French") {
    y.lab <- "Biomasse KT"
    legend.text <- c("BSR-tonne", "confiance à 95%", "NRS-tonne", "NRL-tonne")
  }

  plot(ts.value ~ year, data = tr.df, type = "n", axes = FALSE, xlab = "", ylab = "", ylim = yl)

  ## Vulnerable Biomass
  ## lower and upper
  yrs<-tr.df$year[tr.df$ts.name == "HSpredlow"]
  cil<-tr.df$ts.value[tr.df$ts.name == "HSpredlow"]
  cih<-tr.df$ts.value[tr.df$ts.name == "HSpredhigh"]

  polygon(  x = c(yrs,rev(yrs)),y = c(cil,rev(cih)), col = 'grey', border = NA )
  #lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSpredlow"), ], type = "l", lty = 2)
  #lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSpredhigh"), ], type = "l", lty = 2)
  lines(ts.value ~ year, data = tr.df[which(tr.df$ts.name == "HSpred"), ], type = "l", lwd = 1,col='grey40',lty=1)

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
         lty = c(1, 2, NA, 1, 3, 3),
         lwd = c(1, 1, NA, 3, 2, 2),
         pch = c(NA,NA,16,NA,NA,NA),
         col = c('grey40','grey',"black", "blue", "forestgreen", "red"),
         box.lwd = 0.5
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
    y.lab <- "Mortality"
    legend.text <- c("F-1/yr", "95% confidence", "RR-1/yr", "M-1/yr")
  }
  if (language == "French") {
    y.lab <- "Mortalité"
    legend.text <- c("F-1/yr", "confiance à 95%", "RP-1/yr", "M-1/yr")
  }

  idx <- which(in.df$panel.category == "Fishing" & in.df$ts.name == "F-1/yr")
  plot(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "Ut"), ], type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "", ylim = yl)
  lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "Utlow"), ], type = "l", lty = 2)
  lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "Uthigh"), ], type = "l", lty = 2)
  #lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "Flim-1/yr"), ], lty = 3, lwd = 2, col = "red")
  abline(h=0.0877*c(0.8,1,1.2), lty = 3, lwd = 2)

  #lines(ts.value ~ year, data = bl.df[which(bl.df$ts.name == "M-1/yr"), ], lty = 1, lwd = 2, col = grey(0.5))

  ## natural mortality

  legend("topright","(C)", bty = "n", cex=1.25)
  legend("topleft",
         legend.text,
         lty = c(1, 2, 3, 1),
         lwd = c(2, 1, 2, 2),
         col = c("black", "black", "red", grey(0.5)),
         box.lwd = 0.5
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
    y.lab <- "Recruitment"
    legend.text <- c("R-E06", "95% confidence")
  }
  if (language == "French") {
    y.lab <- "Recrutement"
    legend.text <- c("R-E06", "confiance à 95%")
  }
  plot(ts.value ~ year, data = br.df[which(br.df$ts.name == "R-E06"), ], type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "", ylim = yl)
  lines(ts.value ~ year, data = br.df[which(br.df$ts.name == "Rlow-E06"), ], type = "l", lty = 2)
  lines(ts.value ~ year, data = br.df[which(br.df$ts.name == "Rhigh-E06"), ], type = "l", lty = 2)

  legend("topright","(D)", bty = "n", cex=1.25)
  legend("topleft",
         legend.text,
         lty = c(1, 2),
         lwd = c(2, 1),
         col = c("black", "black"),
         box.lwd = 0.5
  )

  axis(side = 1, padj = -0.5)
  mtext(side = 1, x.lab, line = 2, cex = 0.75)
  axis(side = 2, las = 1, hadj = 0.9)
  mtext(side = 2, y.lab, line = 3, cex = 0.75)
  box()
} # end function definition

