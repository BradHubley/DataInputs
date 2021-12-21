#' @export
# edited by Nell to spit out the data summarized by tub as well as set; default is to summarize by set - if you want data by tub set getrawdata to TRUE

hookDataBase <- function(species=30, datadir, getrawdata=FALSE){

  library(reshape2)
  library(tidyverse)

  get_data_custom(schema="isdb", data.dir = datadir, tables = c("ISTRIPS","ISFISHSETS","ISCATCHES","ISFISHSETGEARGROUPS","ISGEARCOMPONENTS","ISGEARCOMPONENTSBAIT","ISGEARFEATURECLASSES"), usepkg = "roracle", fn.oracle.dsn= "PTRAN", fn.oracle.username=uid, fn.oracle.password =pwd)

  ISTRIPS<-ISTRIPS[ISTRIPS$TRIPCD_ID == 7057,]
  ISFISHSETS<-ISFISHSETS[ISFISHSETS$TRIP_ID %in% ISTRIPS$TRIP_ID & ISFISHSETS$SETCD_ID==5,]
  ISFISHSETGEARGROUPS<-ISFISHSETGEARGROUPS[ISFISHSETGEARGROUPS$FISHSET_ID%in%ISFISHSETS$FISHSET_ID,]
  ISGEARCOMPONENTS<-ISGEARCOMPONENTS[ISGEARCOMPONENTS$SETGEARGROUP_ID%in%ISFISHSETGEARGROUPS$SETGEARGROUP_ID,]

  hook_occupancy<-left_join(ISFISHSETGEARGROUPS[,c("FISHSET_ID","SETGEARGROUP_ID", "GEARGROUP_NO", "GEAR_COMPONENTS_GROUPED")], ISGEARCOMPONENTS[,c("GEARCOMPONENT_ID","SETGEARGROUP_ID", "GEAR_COMPONENT_NO", "CONDITIONCD_ID")]) %>%
    left_join(.,ISCATCHES[,c("GEARCOMPONENT_ID", "SPECCD_ID")])


  hook_occupancy$labels <- NA
  hook_occupancy$labels[hook_occupancy$CONDITIONCD_ID==0] = "empty_unbaited"
  hook_occupancy$labels[hook_occupancy$CONDITIONCD_ID==1] = "empty_baited"
  hook_occupancy$labels[hook_occupancy$CONDITIONCD_ID==2&hook_occupancy$SPECCD_ID==species] = "target_species"
  hook_occupancy$labels[hook_occupancy$CONDITIONCD_ID==2&hook_occupancy$SPECCD_ID!=species] = "other_species"
  hook_occupancy$labels[hook_occupancy$CONDITIONCD_ID==4] = "missing_hook"
  hook_occupancy$labels[hook_occupancy$CONDITIONCD_ID==5] = "missing_hook"
  hook_occupancy$labels[hook_occupancy$CONDITIONCD_ID==6] = "broken_hook"

  hook_occupancy <- subset(hook_occupancy,!is.na(labels)) # these hooks indicate something was caught but no

  hook_data <- hook_occupancy %>% group_by(FISHSET_ID) %>% count(labels) %>% pivot_wider(names_from = labels,values_from = n) %>% data.frame()
  hook_data[is.na(hook_data)]<-0
  hook_data$total_sampled=rowSums(hook_data[,c("empty_baited", "empty_unbaited", "other_species",  "target_species", "missing_hook", "broken_hook")])

  # crate summary tub by tub
  raw_tub_data <- hook_occupancy %>% group_by(FISHSET_ID,GEAR_COMPONENT_NO) %>% count(labels) %>% pivot_wider(names_from = labels,values_from = n) %>% data.frame()
  raw_tub_data[is.na(raw_tub_data)]<-0
  raw_tub_data$total_sampled=rowSums(raw_tub_data[,c("empty_baited", "empty_unbaited", "other_species",  "target_species", "missing_hook", "broken_hook")])

  if(getrawdata==FALSE){return(hook_data)}
  if(getrawdata==TRUE){return(raw_tub_data)}


}

