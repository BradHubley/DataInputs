#' @export
OCdata<-function(datadir){

  OCtows<-read_csv(file.path(datadir,"FisheryData/OceanChoice/OC_AtlanticHalibut3NO3Ps4Vn4Vs2023-2025.csv"))
  OCtows$Date<-as.Date(OCtows$Haul[1],"%m/%d/%Y")
  OCtows$Tow<-OCtows$`Haul no.`
  OCtows$slon<-OCtows$`Lon (Begin)`
  OCtows$slat<-OCtows$`Lat (Begin)`
  OCtows$elon<-OCtows$`Lon (End)`
  OCtows$elat<-OCtows$`Lat (End)`
  OCtows$NAFO<-OCtows$`Main fishing zone`

  OCtows<- subset(OCtows,Species=="Halibut Atlantic"&NAFO%in%c("3N","3O","3Ps","4Vs"))

  bioMap(ylim=c(42.5, 48),xlim = c(-60, -47),nafo=T)
  with(OCtows,segments(slon,slat,elon,elat,col=rgb(1,0,0,0.3)))


  # Specify the directory path
  fp=file.path(datadir,"FisheryData/OceanChoice/LengthSamples")
  # Get the list of file names
  file_names <- list.files(fp)
  new_file_names<-c()
  LSfiles<-list()
  # Import the files
  for(i in 1:length(file_names)){

    LSfiles[[i]] <- na.omit(read_excel(file.path(fp,file_names[i]),skip=5)[,c(1,4:5)])
    new_file_names[i] <- unlist(read_excel(file.path(fp,file_names[i]),range="B3",col_names = F))
    LSfiles[[i]]$Vessel<-ifelse(substr(new_file_names[i],1,1)=="C","Calvert","Katsheshuk")
    LSfiles[[i]]$Trip<-new_file_names[i]

    if(is.character(LSfiles[[i]]$Date)) LSfiles[[i]]$Date<-as.Date(as.numeric(LSfiles[[i]]$Date),origin="1900-01-01")
    LSfiles[[i]]$Date<-as.Date(LSfiles[[i]]$Date)
    LSfiles[[i]]$Tow<-as.numeric(LSfiles[[i]]$Tow)
    LSfiles[[i]]$CM<-as.numeric(LSfiles[[i]]$CM)
  }
  OCLengthSamples<-do.call("rbind",LSfiles)
  OCLengthSamples$Trip.Tow<-paste( OCLengthSamples$Trip, OCLengthSamples$Tow)
  #names(LSfiles)<-new_file_names

  #with(OCLengthSamples,hist(CM,breaks=100))

  return(list(OCtows,OCLengthSamples))


}
