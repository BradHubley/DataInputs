
# 4 functions


# Function 3 :
# get 21B Atlantic halibut landing
# download NAFO landing 21B data:  https://www.nafo.int/Data/Catch-Statistics
# type=1 by Div gear
# type =2 (by div),
# type =3(catch by gear across all years/div)
# type = 4(SCAL format: Year,LL3, LL4, OT3, OT4), 'Landings',
# zone 5 ("5ZC","5ZE","5Y") is restricted to CDN landings only
#' @export


get_21B <- function(count="CDN",yearstart=1970, yearend=2024, type=1, datadir){
  nafo.70.79 <- read.csv(file.path(datadir,"NAFO21B-70-79.txt"), header = TRUE)
  nafo.80.89 <- read.csv(file.path(datadir,"NAFO21B-80-89.txt"), header = TRUE)
  nafo.90.99 <- read.csv(file.path(datadir,"NAFO21B-90-99.txt"), header = TRUE)
  nafo.00.09 <- read.csv(file.path(datadir,"NAFO21B-2000-09.txt"), header = TRUE)
  nafo.10.19 <- read.csv(file.path(datadir,"NAFO-21B-2010-2019.txt"), header = TRUE)
  nafo.20.23 <- read.csv(file.path(datadir,"NAFO-21B-2020-2023.txt"), header = TRUE)
  #names(nafo.00.09)[9]<-"Catches"
  #names(nafo.10.18)[9]<-"Catches"
  names(nafo.10.19)[3]<-"GearCode"
  names(nafo.10.19)[6]<-"Divcode"
  names(nafo.10.19)[7]<-"Code"
  sort(unique(nafo.10.18$Divcode))

  division <- read.csv(file.path(datadir,"divisions.txt"), header = F)
  colnames(division)=c("Divcode", "Div")
  gear <- read.csv(file.path(datadir,"gear.txt"), header = F)
  colnames(gear)=c("GearName","GearCode", "Gear")
  species <- read.csv(file.path(datadir,"species.txt"), sep="",header = F)  # A halibut code 120
  # divisions of interest
  divCAN<-division$Divcode[division$Div%in%c("3N","3O","3P","3PS","3NK","4V","4VN","4VS","4W","4X","4NK","5Y","5Z","5ZE","5ZC")]

  #get gear and divsion data for the landing
  nafoall<-rbind(nafo.70.79, nafo.80.89, nafo.90.99, nafo.00.09, nafo.10.19, nafo.20.23)
  nafoall = merge(nafoall, division)
  nafoall = merge(nafoall, gear)

  # estimate annual catch as a sum of monthly catch
  #nafoall$total<-rowSums(nafoall[,10:21])
  nafoall$total<-rowSums(apply(nafoall[,10:21],2,as.numeric),na.rm=T) # did this to deal with some bad data in NAFO-21B-2010-18.txt

  #filter divisions; zone 5 includes CDN landings only in "5ZC","5ZE","5Y"
  nafoAH5 = nafoall   %>%
    filter (Code=="120",Divcode%in%divCAN,Div%in%c("5ZC","5ZE","5Y"),Country%in%c(2,3,27,28,39,40)) ##39+40...no data in them but still include? incase it ever happens??
  nafoAH34 = nafoall   %>%
    filter (Code=="120",Divcode%in%divCAN,!Div%in%c("5ZC","5ZE","5Y") )
  # unique(nafoAH34$Div)
  nafoAH=rbind(nafoAH34, nafoAH5)

  #select species-A halibut, divisions-areas of 3,4,5,

  if (count=="CDN") {
    nafoAH = nafoAH %>%
    filter (Country %in% c(2,3,27,28) )

  }

  # NAFO 21B all country landing by year/division/gear(>10)
  # manage areas: 5ZC, 5ZE,5Y assigned to 4X
  # 3NK only has 2004 (not known), 2016(OT)of minor catch. assign3NK to 3N - to check in new years of data

  nafoB1=nafoAH  %>%
    dplyr::select(Year, Div,GearName,total )  %>%
    filter(Year >= yearstart)  %>%
    mutate( Division=replace(Div, Div=="5ZC", "4X")) %>%
    mutate( Division=replace(Division, Div=="5ZE", "4X"))  %>%
    mutate( Division=replace(Division, Div=="5Y", "4X")) %>%
    mutate( Division=replace(Division, Div=="3NK", "3N"), Div=NULL)  %>%
    rename(Gear=2)
  # sort(unique(nafoB1$Division))
  # sort(unique(nafoB1$Year))
  #  sort(unique(nafoB1$Gear))

  # catch by year/division;

  nafoBdiv=nafoB1  %>%
    dplyr::select(Year, Division,Gear,total )  %>%
    group_by(Year,Division) %>%
    summarise(CatchB=sum(total))

  nafoB.gear=nafoB1  %>%
    dplyr::select(Year, Division,Gear,total )  %>%
    group_by(Gear) %>%
    summarise(Totalgear=sum(total)) %>%
    arrange(-Totalgear)
  # view(nafoB.gear)
  # sort(unique(nafoB$Gear))

  # LL: Longlines (charters);Longlines (not specified),Set lines; Drift lines (drifting longlines)
  # OT: "Bottom otter trawl";"Bottom otter trawl (charters)";"Bottom otter trawl (side or stern not specified)"; "Bottom otter trawl (side)"
  ## LL and OT are consistent with "Notes on Halibut Landings Data" by Scott In DriveR
  nafoB = nafoB1  %>%
    mutate_if( grepl('Bottom otter',.), ~replace(., grepl('Bottom otter', .), "OT"))%>%
    mutate_if( grepl('Set lines',.), ~replace(., grepl('Set lines', .), "LL"))%>%
    mutate_if( grepl('Longlines',.), ~replace(., grepl('Longlines', .), "LL"))%>%
    mutate_if( grepl('longlines',.), ~replace(., grepl('longlines', .), "LL"))%>%
    mutate( Gear=replace(Gear, !Gear %in% c("OT", "LL"),"Other"))%>%
    group_by(Year, Division, Gear) %>%
    summarise(Catch=sum(total))

  Division=rep(sort(unique(nafoB$Division)),each=3,times=yearend-1970+1)
  Year=rep(c(1970:yearend), each=21)
  Gear=rep(c("LL", "OT", "Other"), times=(yearend-1970+1)*7)
  Divyear70=cbind(Year, Division, Gear)

  #21B catch by Division and gear
  nafoB = merge(nafoB, Divyear70, all.y = T)

  if (type==1 ){
    retdata=nafoB

  } else if (type==2 ){
    retdata=nafoBdiv

  } else if (type==3 ){
    retdata=nafoB.gear

  } else {
    retdata3 = nafoB   %>%
      filter(grepl("3", Division))     %>%
      group_by(Year,Gear)   %>%
      mutate(Catch3 = sum(Catch)  ) %>%
      filter(Gear!="Other")  %>%
      dplyr::select(Year, Gear, Catch3)
    retdata3  = unique(retdata3 ) %>%
      pivot_wider(names_from = Gear, values_from=Catch3) %>%
      rename(LL3=2, OT3=3)

    retdata4 = nafoB   %>%
      filter(grepl("4", Division))     %>%
      group_by(Year,Gear)   %>%
      mutate(Catch4 = sum(Catch)  ) %>%
      filter(Gear!="Other")  %>%
      dplyr::select(Year, Gear, Catch4)

    retdata4  = unique(retdata4 ) %>%
      pivot_wider(names_from = Gear, values_from=Catch4) %>%
      rename(LL4=2, OT4=3)

    retdata= cbind(retdata3,retdata4) %>%
      rename(Year=1  )%>%
      dplyr::select(Year, LL3, LL4, OT3, OT4)
  }

 return(retdata)

}

# Function 4 :
get_SCALformat <- function(landingdata){
    retdata3 = landingdata   %>%
      filter(grepl("3", Division))     %>%
      group_by(Year,Gear)   %>%
      mutate(Catch3 = sum(Catch)  ) %>%
      filter(Gear!="Other")  %>%
      dplyr::select(Year, Gear, Catch3)
    retdata3  = unique(retdata3 ) %>%
      pivot_wider(names_from = Gear, values_from=Catch3) %>%
      rename(LL3=2, OT3=3)

    retdata4 = landingdata   %>%
      filter(grepl("4", Division))     %>%
      group_by(Year,Gear)   %>%
      mutate(Catch4 = sum(Catch)  ) %>%
      filter(Gear!="Other")  %>%
      dplyr::select(Year, Gear, Catch4)

    retdata4  = unique(retdata4 ) %>%
      pivot_wider(names_from = Gear, values_from=Catch4) %>%
      rename(LL4=2, OT4=3)

    retdata= cbind(retdata3,retdata4) %>%
      rename(Year=1  )%>%
      dplyr::select(Year, LL3, LL4, OT3, OT4)

return(retdata)

}


# sort(unique(mar$Division))

