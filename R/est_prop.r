# Function 1 : Estimate proportion by gear
# avg=1 for MARFIS; 3 or 4 for nafo 21B
# missing means the missing year
#'@export

est_prop <- function(landdata, avg, missing){

  if(avg==1) {
    retdata = as.data.frame(landdata)    %>%
      rename(Catch=4)  %>%
      group_by(Year, Division) %>%
      mutate(Catchdiv = sum(Catch))%>%
      arrange(Year) %>%
      mutate(Prop=Catch/Catchdiv) %>%
      dplyr::select(Year, Division, Gear, Prop)

  } else {

    landdata = landdata    %>%
      arrange(Division, Gear, Year)  %>%
      group_by(Division, Gear, Year)

    landm = landdata    %>%
      filter(Year<missing & Year>= (missing-avg))  %>%
      mutate(Year = missing) %>%
      group_by(Division) %>%
      mutate(CatchT = sum(Catch)) %>%
      group_by(Division,Gear) %>%
      mutate(CatchG = sum(Catch)) %>%
      dplyr::select(Year, Division, Gear, CatchG,CatchT)

    retdata = unique(landm)  %>%
      mutate(Prop=CatchG/CatchT) %>%
      dplyr::select(Year, Division, Gear, Prop)
  }
  return (retdata)

}

