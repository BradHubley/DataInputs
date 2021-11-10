#' @name make.fs.index
#' @title Makes ...
#'
#' @description \code{make.fs.index} makes ...
#' @param dat The data
#' @importFrom MASS glm.nb
#' @export



makeOldFSIndex <- function(dat,do.plot=T,pr.stn='33')
{
  #this is Kurt's model with hook number as offset
  #zero catches included (much larger error than not including 0)
  #no culling of data based on the protocol...but there was removal of sets that were 179 min or less, or number of hooks less than 500
  #arbitrary station (33) chosen to predict
  #index is the exponentiated fit
  #code is generalized to flexible for number of years
  
  #dat<-fs.hal.catch
  new <- dat
  new$NK_HOOKS <-new$NUM_HOOK_HAUL/1000
  
  ##limit data to soak time bewtween 3 and 18 hours
  new$DUR_41[is.na(new$DUR_41)] <- 600 ## assume missing data followed the protocol
  new$NK_HOOKS[is.na(new$NK_HOOKS)] <- 1  ## assume missing data followed the protocol
  new <- new[new$DUR_41 >= 179, ]
  new <- new[new$NK_HOOKS >= 0.5, ]
  
  #remove any station not fished in more than 2 years  ###Removed for update, adding new stations does confuse things
  #x<-table(new$STATION)
  #rm.stat<-as.numeric(names(x[x<=4]))
  #length(unique(new$STATION))
  
  #new<-new[!new$STATION%in%rm.stat,]
  #length(unique(new$STATION))
  
  #model
  new$year <- as.factor(new$YEAR)
  new$station <- as.factor(new$STATION)
  new$bothwgt <- new$EST_COMBINED_WT
  new$hooks <- new$NK_HOOKS
  new$DUR_41 <- new$DUR_41
  
  a1 <- MASS::glm.nb(bothwgt ~ year + station + offset(log(hooks)), data = new)
  summary(a1)
  anova(a1)
  
  xv <- sort(unique(new$YEAR))
  #calculate CI for one predicted line
  #say for p.s1
  pr <- predict(a1, data.frame(year = as.factor(xv), hooks = rep(1, length(xv)), station = rep(pr.stn, length(xv))), se.fit = T)
  
  family <- family(a1)
  lower.HS <- family$linkinv(pr$fit - qnorm(0.975) * pr$se.fit)  #NDH Nov 2016 update from 90 to 95%CI
  upper.HS <- family$linkinv(pr$fit + qnorm(0.975) * pr$se.fit)  #NDH Nov 2016 update from 90 to 95%CI
  pr.HS <- family$linkinv(pr$fit)
  
  ylim1 <- c(min(c(lower.HS, lower.HS)), max(c(upper.HS, upper.HS)))
  xlim1 <- range(xv)
  
  if(do.plot==T){
    plot(xv, pr.HS, type = 'l', lty = 1, ylim = ylim1, xlim = xlim1, xlab = '', ylab = '', lwd = 2.5, las = 1)
    lines(xv, upper.HS, lty = 2, lwd = 3)
    lines(xv, lower.HS, lty = 2, lwd = 3)
    mtext("Year", side = 1, line = 2.5, cex = 1.2)
    mtext("Standardized catch rate", side = 2, line = 2.5, cex = 1.2, las = 0)
    mtext("3NOPs4VWX Halibut survey", side = 3, line = .5, cex = 1.5, las = 0, adj = 0)
  }
  
  fs.index <- as.data.frame(cbind(xv, round(pr.HS, 2), round(lower.HS,2), round(upper.HS,2)))
  names(fs.index) <- c('year', 'predicted', 'lowerci', 'upperci')
  
  return(list(index = fs.index, lm = a1, data = new))
  
}
