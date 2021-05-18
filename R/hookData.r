#' @export
hookData <- function(years=2017:2020, species=30,wdir=getwd()){

  library(reshape2)
  library(tidyverse)

  # just a comment that Nell got a browser when she tried this function

  ### read in hook occupancy data
  hook_occupancy_pre2018 <-  NULL

  if(2017 %in% years){

    files_to_get <- list.files(path = file.path(wdir,"Survey/hookoccupancy/2017"))
    hook_occupancy <- NULL
    for(i in 1:length(files_to_get)){
      hook_hold <- read.csv(file = file.path(wdir,"Survey/hookoccupancy/2017", files_to_get[i]), header = TRUE, stringsAsFactors = FALSE)
      hook_occupancy <- rbind(hook_occupancy, hook_hold)
    }
    hook_occupancy_2017 = reshape(hook_occupancy,direction = 'long',varying = list(4:33),timevar='hook')
    colnames(hook_occupancy_2017) <- c("TRIP", "SET_NO","TUB_NO", "HOOK_NO", "HOOK_CONDITION", "SPECCD_ID")
    hook_occupancy_2017$SPECCD_ID <- NA

    #2017 hook codes
    #0 is empty hook - unbaited
    #1 is empty hook - baited
    #2 is broken hook
    #30 is halibut, etc

    hook_occupancy_2017$SPECCD_ID[hook_occupancy_2017$HOOK_CONDITION>2] <- hook_occupancy_2017$HOOK_CONDITION[hook_occupancy_2017$HOOK_CONDITION>2]
    hook_occupancy_2017$HOOK_CONDITION[ hook_occupancy_2017$HOOK_CONDITION == 2] <- 6
    hook_occupancy_2017$HOOK_CONDITION[ !is.na(hook_occupancy_2017$SPECCD_ID)] <- 2
    hook_occupancy_2017$Line_Type=21
    hook_occupancy_2017$GEAR_TYPE=51
    hook_occupancy_2017$YEAR=2017
    #hook_occupancy_2017$HOOKS_PER_TUB=100

    hook_occupancy_pre2018 <- hook_occupancy_2017[c("YEAR","Line_Type", "TRIP", "SET_NO", "GEAR_TYPE", "TUB_NO", "HOOK_NO", "HOOK_CONDITION", "SPECCD_ID")]


  }

  if(2018 %in% years){

    files_to_get <- list.files(path = file.path(wdir,"Survey/hookoccupancy/2018"))
    hook_occupancy_2018 <- NULL
    for(i in 1:length(files_to_get)){
      hook_hold <- read.csv(file = file.path(wdir,"Survey/hookoccupancy/2018", files_to_get[i]), header = F, stringsAsFactors = FALSE)
      hook_occupancy_2018 <- rbind(hook_occupancy_2018, hook_hold)
    }
    colnames(hook_occupancy_2018) <- c("Line_Type", "TRIP", "SET_NO", "GEAR_TYPE", "CAMERA", "NUM_HOOK_HAUL", "NUM_TUBS_SET", "HOOKS_PER_TUB", "TUB_NO", "NUM_FIRST_HOOK_SAMPLED", "SAMPLING_RT", "HOOK_NO", "HOOK_CONDITION", "SPECCD_ID")
    hook_occupancy_2018$YEAR=2018

    hook_occupancy_pre2018 <- rbind(hook_occupancy_pre2018,hook_occupancy_2018[c("YEAR","Line_Type", "TRIP", "SET_NO", "GEAR_TYPE", "TUB_NO", "HOOK_NO", "HOOK_CONDITION", "SPECCD_ID")])

  }

  #The new hook occupancy codes (2018+):
  #0 = empty hook, unbaited
  #1 = empty hook, baited
  #2 = halibut present
  #5 = missing hook
  #6 = damaged or broken hook


  hook_occupancy_lst<-list()

  for(y in 3:length(years)){

    files_to_get <- list.files(path = file.path(wdir,"Survey/hookoccupancy",years[y]))
    hook_occupancy_tmp <- NULL
    for(i in 1:length(files_to_get)){
      hook_hold <- read.fwf(file.path(wdir,"Survey/hookoccupancy",years[y], files_to_get[i]), widths=c(2,10,3,3,3,4,4,3,16))
      hook_occupancy_tmp <- rbind(hook_occupancy_tmp, hook_hold)

    }

    colnames(hook_occupancy_tmp) <- c("Line_Type", "TRIP", "SET_NO", "GEAR_TYPE", "TUB_NO", "HOOKS_PER_TUB", "HOOK_NO", "HOOK_CONDITION", "SPECCD_ID")
    hook_occupancy_tmp$YEAR=years[y]

    hook_occupancy_lst[[y-2]] <- hook_occupancy_tmp

  }

  hook_occupancy <- do.call("rbind",hook_occupancy_lst)[c("YEAR","Line_Type", "TRIP", "SET_NO", "GEAR_TYPE", "TUB_NO", "HOOK_NO", "HOOK_CONDITION", "SPECCD_ID")]
  hook_occupancy <- rbind(hook_occupancy_pre2018,hook_occupancy)

  # clean up
  hook_occupancy$TRIP<-gsub(" ", "", hook_occupancy$TRIP, fixed = TRUE)
  tn<-as.numeric(substr(hook_occupancy$TRIP,6,8))
  hook_occupancy$TRIP[tn<500]<-gsub("H", "J", hook_occupancy$TRIP[tn<500], fixed = TRUE)
  hook_occupancy$TRIP[tn>500]<-gsub("H", "S", hook_occupancy$TRIP[tn>500], fixed = TRUE)
  hook_occupancy <- subset(hook_occupancy,!is.na(HOOK_CONDITION))
  hook_occupancy[hook_occupancy$TRIP == "J17-0271A1", "TRIP"] = "J17-0271A"
  hook_occupancy[hook_occupancy$TRIP == "J17-0271A2", "TRIP"] = "J17-0271B"


  raw_hook_data <-hook_occupancy
  hook_occupancy <- raw_hook_data


  hook_occupancy$labels <- NA
  hook_occupancy$labels[hook_occupancy$HOOK_CONDITION==0] = "empty_unbaited"
  hook_occupancy$labels[hook_occupancy$HOOK_CONDITION==1] = "empty_baited"
  hook_occupancy$labels[hook_occupancy$HOOK_CONDITION==2&hook_occupancy$SPECCD_ID==species] = "target_species"
  hook_occupancy$labels[hook_occupancy$HOOK_CONDITION==2&hook_occupancy$SPECCD_ID!=species] = "other_species"
  hook_occupancy$labels[hook_occupancy$HOOK_CONDITION==4] = "missing_hook"
  hook_occupancy$labels[hook_occupancy$HOOK_CONDITION==5] = "missing_hook"
  hook_occupancy$labels[hook_occupancy$HOOK_CONDITION==6] = "broken_hook"

  hook_occupancy <- subset(hook_occupancy,!is.na(labels)) # these hooks indicate something was caught but no species identified, should be removed?
  #hook_occupancy[is.na(hook_occupancy)]<-0


  hook_data <- hook_occupancy %>% group_by(TRIP,SET_NO) %>% count(labels) %>% pivot_wider(names_from = labels,values_from = n) %>% data.frame()
  hook_data[is.na(hook_data)]<-0
  hook_data$total_sampled=rowSums(hook_data[,3:8])


  return(hook_data)
}


