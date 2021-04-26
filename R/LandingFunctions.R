
# 4 functions

# Function 1 : Estimate proportion by gear
# avg=1 for MARFIS; 3 or 4 for nafo 21B
# missing means the missing year

 est_prop <- function(landdata, avg, missing){
    
    if(avg==1) {
    retdata = as.data.frame(landdata)    %>%
        rename(Catch=4)  %>%
        group_by(Year, Division) %>%
        mutate(Catchdiv = sum(Catch))%>%
        arrange(Year) %>%
        mutate(Prop=Catch/Catchdiv) %>%
        select(Year, Division, Gear, Prop)

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
        select(Year, Division, Gear, CatchG,CatchT)
        
      retdata = unique(landm)  %>%
        mutate(Prop=CatchG/CatchT) %>%
        select(Year, Division, Gear, Prop)
    }
  return (retdata)
   
     }
  

# Function 2 :
# get 21A Atlantic halibut landing, unit ton
# Download from here: https://www.nafo.int/Data/STATLANT. Select HAL - ATLANTIC HALIBUT as the species, select all countries,  and save the .csv
# count="CDN" or others
 
get_21A <- function(count, yearstart){ 
  landA = read.csv("./NAFOlanding/Export.csv")
#  names(landA)
  names(landA)[5]="Catch"
  names(landA)[1]="Year"
  names(landA)[4]="Species"
#  unique(landA$Species)
#  unique(landA$Country)  
  
  # Canada+Foreign landings by year/area; assign Area 5 to 4X
  if (count=="CDN") {
      nafoA = as.data.frame(landA) %>%
          filter(Division %in% c(nafodivs3NOPS, nafodivs4VWX5Z),
          grepl("CAN",landA$Country)) 
  } else {
    
      nafoA = as.data.frame(landA) %>%
          filter(Division %in% c(nafodivs3NOPS, nafodivs4VWX5Z)) 
  }

  
  nafoA = nafoA %>%
    filter(Year >=yearstart)%>%
    mutate( Division=replace(Division, Division=="5ZC", "4X")) %>%
    mutate( Division=replace(Division, Division=="5ZE", "4X"))  %>% 
    mutate( Division=replace(Division, Division=="5Z", "4X"))  %>%
    mutate( Division=replace(Division, Division=="5Y", "4X")) %>%
    mutate( Division=replace(Division, Division=="3NK", "3N"))   %>%  
    group_by(Year, Division) %>%
    summarize(CatchA=sum(Catch))

  nafoA=data.frame(nafoA)
  sort(unique(nafoA$Division))
  
  return(nafoA)
}

# Function 3 :
# get 21B Atlantic halibut landing
# download NAFO landing 21B data:  https://www.nafo.int/Data/Catch-Statistics
# type=1 by Div gear
# type =2 (by div),
# type =3(catch by gear across all years/div)
# type = 4(SCAL format: Year,LL3, LL4, OT3, OT4)


get_21B <- function(count="CDN",yearstart=1970, yearend=2016, type=1){ 
  nafo.70.79 <- read.csv("./NAFOlanding/NAFO21B-70-79.txt", header = TRUE)
  nafo.80.89 <- read.csv("./NAFOlanding/NAFO21B-80-89.txt", header = TRUE)
  nafo.90.99 <- read.csv("./NAFOlanding/NAFO21B-90-99.txt", header = TRUE)
  nafo.00.09 <- read.csv("./NAFOlanding/NAFO21B-2000-09.txt", header = TRUE)
  nafo.10.16 <- read.csv("./NAFOlanding/NAFO-21B-2010-16.txt", header = TRUE)
  names(nafo.00.09)[9]<-"Catches"
  names(nafo.10.16)[9]<-"Catches"
  names(nafo.10.16)[3]<-"GearCode"
  names(nafo.10.16)[6]<-"Divcode"
  names(nafo.10.16)[7]<-"Code"
  sort(unique(nafo.10.16$Divcode))
  
  division <- read.csv("./NAFOlanding/divisions.txt", header = F)
  colnames(division)=c("Divcode", "Div")
  gear <- read.csv("./NAFOlanding/gear.txt", header = F)
  colnames(gear)=c("GearName","GearCode", "Gear")
  species <- read.csv("./NAFOlanding/species.txt", sep="",header = F)  # A halibut code 120 
  # divisions of interest
  divCAN<-division$Divcode[division$Div%in%c("3N","3O","3P","3PS","3NK","4V","4VN","4VS","4W","4X","4NK","5Y","5Z","5ZE","5ZC")]
  
  #get gear and divsion data for the landing
  nafoall<-rbind(nafo.70.79, nafo.80.89, nafo.90.99, nafo.00.09, nafo.10.16)
  nafoall = merge(nafoall, division)
  nafoall = merge(nafoall, gear)
  
  #selct species-A halibut, divisions-areas of 3,4,5, 
  if (count=="CDN") {
    nafoAH = nafoall %>%
    filter (Code=="120",Divcode%in%divCAN, Country %in% c(2,3,27,28) ) 
    
  }  else {
    
    nafoAH = nafoall %>%
    filter (Code=="120",Divcode%in%divCAN ) 
  }
  
  nafoAH$total<-rowSums(nafoAH[,10:21])

  # NAFO 21B all country landing by year/division/gear(>10) 
  # manage areas: 5ZC and 5ZE (CDN catch),5Y assigned to 4X
  # 3NK only has two years of minor catch. assign3NK to 3N
 
  nafoB1=nafoAH  %>%
    select(Year, Div,GearName,total )  %>%
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
    select(Year, Division,Gear,total )  %>%
    group_by(Year,Division) %>%
    summarise(CatchB=sum(total)) 
  
  nafoB.gear=nafoB1  %>%
    select(Year, Division,Gear,total )  %>%
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
      select(Year, Gear, Catch3)
    retdata3  = unique(retdata3 ) %>%
      pivot_wider(names_from = Gear, values_from=Catch3) %>%
      rename(LL3=2, OT3=3)
    
    retdata4 = nafoB   %>%
      filter(grepl("4", Division))     %>%
      group_by(Year,Gear)   %>%
      mutate(Catch4 = sum(Catch)  ) %>%
      filter(Gear!="Other")  %>%
      select(Year, Gear, Catch4)
    
    retdata4  = unique(retdata4 ) %>%
      pivot_wider(names_from = Gear, values_from=Catch4) %>%
      rename(LL4=2, OT4=3)
    
    retdata= cbind(retdata3,retdata4) %>%
      rename(Year=1  )%>%
      select(Year, LL3, LL4, OT3, OT4)
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
      select(Year, Gear, Catch3)
    retdata3  = unique(retdata3 ) %>%
      pivot_wider(names_from = Gear, values_from=Catch3) %>%
      rename(LL3=2, OT3=3)
    
    retdata4 = landingdata   %>%
      filter(grepl("4", Division))     %>%
      group_by(Year,Gear)   %>%
      mutate(Catch4 = sum(Catch)  ) %>%
      filter(Gear!="Other")  %>%
      select(Year, Gear, Catch4)
    
    retdata4  = unique(retdata4 ) %>%
      pivot_wider(names_from = Gear, values_from=Catch4) %>%
      rename(LL4=2, OT4=3)
    
    retdata= cbind(retdata3,retdata4) %>%
      rename(Year=1  )%>%
      select(Year, LL3, LL4, OT3, OT4)

return(retdata)

}


# sort(unique(mar$Division))

