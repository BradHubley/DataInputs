# get 21B Atlantic halibut landing
# download NAFO landing 21B data:  https://www.nafo.int/Data/Catch-Statistics

#' @export


get_21B <- function(datadir, yearstart=1970){

  file.names<-list.files(file.path(datadir,"Landings","Table21B"))
  files<-list()

  for(i in 1:length(file.names)){
    files[[i]]<-read.csv(file.path(datadir,"Landings","Table21B",file.names[i]), header = TRUE)

    names(files[[i]])<-names(files[[1]])

  }
  nafoall<-do.call("rbind",files)

  division <- read.csv(file.path(datadir,"divisions.txt"), header = F)
  colnames(division)=c("AreaCode", "Div")
  divCAN<-division$AreaCode[division$Div%in%c("3N","3O","3P","3PS","3NK","4V","4VN","4VS","4W","4X","4NK","5Y","5Z","5ZE","5ZC")]

  gear <- read.csv(file.path(datadir,"gear.txt"), header = F)
  colnames(gear)=c("GearName","Gear", "abv")

  nafoall = merge(nafoall, division)
  nafoall = merge(nafoall, gear)
  mths<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  nafoall$total<-rowSums(apply(nafoall[,mths],2,as.numeric),na.rm=T) # did this to deal with some bad data in NAFO-21B-2010-18.txt

  #filter divisions; zone 5 includes CDN landings only in "5ZC","5ZE","5Y"
  nafoAH5 = nafoall   %>%
    filter (SpeciesEffort =="120",AreaCode%in%divCAN,Div%in%c("5ZC","5ZE","5Y"),Country%in%c(2,3,27,28,39,40)) ##39+40...no data in them but still include? incase it ever happens??
  nafoAH34 = nafoall   %>%
    filter (SpeciesEffort =="120",AreaCode%in%divCAN,!Div%in%c("5ZC","5ZE","5Y") )
  # unique(nafoAH34$Div)
  nafoAH=rbind(nafoAH34, nafoAH5)

  #select species-A halibut, divisions-areas of 3,4,5,



  # NAFO 21B all country landing by year/division/gear(>10)
  # manage areas: 5ZC, 5ZE,5Y assigned to 4X
  # 3NK only has 2004 (not known), 2016(OT)of minor catch. assign3NK to 3N - to check in new years of data

  nafoB1=nafoAH  %>%
    dplyr::select(Year, Div,GearName,total )  %>%
    filter(Year >= yearstart)  %>%
    mutate( Division=replace(Div, Div%in%c("5ZC","5ZE","5Y"), "4X"))  %>%
    mutate( Division=replace(Division, Div=="3NK", "3N"), Div=NULL)  %>%
    rename(Gear=2)
  # sort(unique(nafoB1$Division))
  # sort(unique(nafoB1$Year))
  #  sort(unique(nafoB1$Gear))

  # sort(unique(nafoB$Gear))

  # LL: Longlines (charters);Longlines (not specified),Set lines; Drift lines (drifting longlines)
  # OT: "Bottom otter trawl";"Bottom otter trawl (charters)";"Bottom otter trawl (side or stern not specified)"; "Bottom otter trawl (side)"
  ## LL and OT are consistent with "Notes on Halibut Landings Data" by Scott In DriveR
  nafoB = nafoB1  %>%
    mutate_if( grepl('Bottom otter',.), ~replace(., grepl('Bottom otter', .), "OT"))%>%
    mutate_if( grepl('trawl',.), ~replace(., grepl('trawl', .), "OT"))%>%
    mutate_if( grepl('Set lines',.), ~replace(., grepl('Set lines', .), "LL"))%>%
    mutate_if( grepl('Longlines',.), ~replace(., grepl('Longlines', .), "LL"))%>%
    mutate_if( grepl('longlines',.), ~replace(., grepl('longlines', .), "LL"))%>%
    mutate_if( grepl('lines',.), ~replace(., grepl('lines', .), "LL"))%>%
    mutate( Gear=replace(Gear, !Gear %in% c("OT", "LL"),"Other"))%>%
    group_by(Year, Division, Gear) %>%
    summarise(Catch=sum(total)) %>%
    as.data.frame()

  #Division=rep(sort(unique(nafoB$Division)),each=3,times=yearend-1970+1)
  #Year=rep(c(1970:yearend), each=21)
  #Gear=rep(c("LL", "OT", "Other"), times=(yearend-1970+1)*7)
  #Divyear70=data.frame(Year, Division, Gear)

  #21B catch by Division and gear
  #nafoB = merge(nafoB, Divyear70, all.y = T)
  g<-expand.grid(Year=min(nafoB$Year):max(nafoB$Year),Division=sort(unique(nafoB$Division)),Gear=c("LL","OT"))
  nafoB<-merge(g,nafoB,all.x=T)
  nafoB$Catch[is.na(nafoB$Catch)]<-0


 return(nafoB)

}




# sort(unique(mar$Division))

