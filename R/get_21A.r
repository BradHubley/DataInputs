# Function 2 :
# get 21A Atlantic halibut landing, unit ton
# Download from here: https://www.nafo.int/Data/STATLANT-21A. Select HAL - ATLANTIC HALIBUT as the species, select all countries,  and save the .csv
# count="CDN" for Canada; "Foreign" for foreign countries; others for all countries
# zone 5 ("5ZC","5ZE","5Z","5Y") is restricted to CDN landings only
# landings in "5Z" only occurred in 1960s.
#' @export


get_21A <- function(count, yearstart, datadir, fn="Export.csv"){

  nafodivs3NOPS = c("3N","3O","3P","3PS","3NK")
  nafodivs4VWX5Z = c("4V","4VN","4VS","4W","4X","4NK","5Y","5Z","5ZE","5ZC")

  landA = read.csv(file.path(datadir,fn))
  #  names(landA)
  names(landA)[5]="Catch"
  names(landA)[1]="Year"
  names(landA)[4]="Species"
  #  unique(landA$Species)
  #  unique(landA$Country)

  #filter divisions; zone 5 includes CDN landings only
  landA = as.data.frame(landA) %>%
    dplyr::filter(Division %in% c(nafodivs3NOPS, nafodivs4VWX5Z))
  landA5 =   landA   %>%
    dplyr::filter(Division %in% c("5ZC","5ZE","5Z","5Y"),
           grepl("CAN",landA$Country))
  landA34 =   landA   %>%
    dplyr::filter(!Division %in% c("5ZC","5ZE","5Z","5Y"))

  landA=rbind(landA34, landA5)
#  unique(landA5$Country)


  # Canada+Foreign landings by year/area; assign Area 5 to 4X
  if (count=="CDN") {
    nafoA = landA %>%
      dplyr::filter(Division %in% c(nafodivs3NOPS, nafodivs4VWX5Z),
             grepl("CAN",landA$Country))
  } else if (count=="Foreign") {
      nafoA = landA %>%
        dplyr::filter(Division %in% c(nafodivs3NOPS, nafodivs4VWX5Z),
          !grepl("CAN",landA$Country))

    } else {

    nafoA = landA
  }


  nafoA = nafoA %>%
    dplyr::filter(Year >=yearstart)%>%
    #mutate( Division=replace(Division, Division%in%c("5ZC","5ZE","5Y"), "4X"))  %>%
    mutate( Division=replace(Division, Division=="3NK", "3N"))   %>%
    group_by(Year, Division) %>%
    summarize(CatchA=sum(Catch))

  nafoA=data.frame(nafoA)
  sort(unique(nafoA$Division))

  return(nafoA)
}

