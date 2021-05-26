# Function 2 :
# get 21A Atlantic halibut landing, unit ton
# Download from here: https://www.nafo.int/Data/STATLANT. Select HAL - ATLANTIC HALIBUT as the species, select all countries,  and save the .csv
# count="CDN" for Canada; "Foreign" for foreign countries; others for all countries
#'@export

get_21A <- function(count, yearstart, wd=getwd()){
  landA = read.csv(file.path(wd,'data',"Export.csv"))
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
  } else if (count=="Foreign") {
      nafoA = as.data.frame(landA) %>%
          filter(Division %in% c(nafodivs3NOPS, nafodivs4VWX5Z),
          !grepl("CAN",landA$Country))
   
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

