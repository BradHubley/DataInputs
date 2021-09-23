#' @export
getOldScalInputs<-function(scaldir=file.path(datadir,"scal")){

  ctl<-lisread(file.path(scaldir,'scal_control.ctl'))

  catch<-lisread(file.path(scaldir,'scal_catch.dat'))

  index<-lisread(file.path(scaldir,'scal_index.dat'))
  index$idxSeries <- cbind(index$RV_4VWX,index$HS)

  lengths<-lisread(file.path(scaldir,'scal_lengths.dat'))
  lengths$lfBins<-seq(lengths$minLen[1],lengths$maxLen[1],lengths$binSize)
  lengths$startBins_m<-lengths$startBins[1:6,]
  lengths$startBins_f<-lengths$startBins[7:12,]
  lengths$startBins_c<-lengths$startBins[13:18,]
  lengths$endBins_m<-lengths$endBins[1:6,]
  lengths$endBins_f<-lengths$endBins[7:12,]
  lengths$endBins_c<-lengths$endBins[13:18,]
  lengths$nObsBins_m<-lengths$nObsBins[1:6,]
  lengths$nObsBins_f<-lengths$nObsBins[7:12,]
  lengths$nObsBins_c<-lengths$nObsBins[13:18,]
  lengths$lenObsProp_m <- with(lengths,array(data=c(lenObsProp_m1,lenObsProp_m2,lenObsProp_m3,lenObsProp_m4,lenObsProp_m5,lenObsProp_m6),dim=c(length(lfBins),ctl$nT,6)))
  lengths$lenObsProp_f <- with(lengths,array(data=c(lenObsProp_f1,lenObsProp_f2,lenObsProp_f3,lenObsProp_f4,lenObsProp_f5,lenObsProp_f6),dim=c(length(lfBins),ctl$nT,6)))
  lengths$lenObsProp_c <- with(lengths,array(data=c(lenObsProp_c1,lenObsProp_c2,lenObsProp_c3,lenObsProp_c4,lenObsProp_c5,lenObsProp_c6),dim=c(length(lfBins),ctl$nT,6)))
  lengths$PropFemale <- with(lengths,array(data=c(PropFemale1,PropFemale2,PropFemale3,PropFemale4,PropFemale5,PropFemale6),dim=c(length(lfBins),ctl$nT,6)))
  #proj<-lisread(file.path(getwd(),'data','scal_proj.dat'))

  data_input = c(
    ctl[c('nT', 'nFisheries', 'nAges', 'baranovIter', 'domeSelectivity', 'dM', 'priorMean_h','priorSD_h', 'prior_initM', 'priorSD_initM','priorSD_R','aMat_50','aMat_95','lInf_m','lInf_f','vonK_m','vonK_f','wt_a','wt_b','cvL_m','cvL_f')],
    catch,
    index[c("nIndexSeries","idxSeries", "idxRelative", "idxNumbers", "idxLikeWeight", "idxFirstYear", "idxLastYear",  "fracYearSurvey", "idxSeries", "fTag")],
    lengths[c("nLenSeries", "nLenBins", "firstBin", "lastBin", "binSize", "sizeLimit","lfBins", "lenIndex", "lenLikeWeight", "minLen", "maxLen", "lenFirstYear", "lenLastYear", "startBins_m", "startBins_f", "startBins_c", "endBins_m", "endBins_f", "endBins_c","nObsBins_m", "nObsBins_f", "nObsBins_c", "lenObsProp_m", "lenObsProp_f", "lenObsProp_c","PropFemale")]
  )

  return(data_input)
}
