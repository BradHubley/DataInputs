# Function 2 :
# get 21A Atlantic halibut landing, unit ton
# Download from here: https://www.nafo.int/Data/STATLANT. Select HAL - ATLANTIC HALIBUT as the species, select all countries,  and save the .csv
# count="CDN" for Canada; "Foreign" for foreign countries; others for all countries
# zone 5 ("5ZC","5ZE","5Z","5Y") is restricted to CDN landings only
# landings in "5Z" only occurred in 1960s.
#' @export


get_21A_bh <- function(count, yearstart,datadir){



  landA = read.csv(file.path(datadir,"Export.csv"))
  #  names(landA)
  names(landA)[ncol(landA)]="Catch"
  names(landA)[1]="Year"
  #names(landA)[4]="Species"
  #  unique(landA$Species)
  #  unique(landA$Country)

  #filter divisions; zone 5 includes CDN landings only
  landA = as.data.frame(landA) %>%
    filter(Division %in% c(nafodivs3NOPS, nafodivs4VWX5Z))
  landA5 =   landA   %>%
    filter(Division %in% c("5ZC","5ZE","5Z","5Y"),
           grepl("CAN",landA$Country))
  landA34 =   landA   %>%
    filter(!Division %in% c("5ZC","5ZE","5Z","5Y"))

  landA=rbind(landA34, landA5)
#  unique(landA5$Country)


  # Canada+Foreign landings by year/area; assign Area 5 to 4X
  if (count=="CDN") {
    nafoA = landA %>%
      filter(Division %in% c(nafodivs3NOPS, nafodivs4VWX5Z),
             grepl("CAN",landA$Country))
  } else if (count=="Foreign") {
      nafoA = landA %>%
          filter(Division %in% c(nafodivs3NOPS, nafodivs4VWX5Z),
          !grepl("CAN",landA$Country))

    } else {

    nafoA = landA
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

