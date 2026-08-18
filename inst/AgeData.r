library(tidyverse)
source(file.path(getwd(), "directories.r"))
source(file.path(wd, "passwords.r"))

ageData<-read.csv(file.path(datadir,"AgeData","ageData.csv"))
head(ageData)
summary(ageData)
plot(LENGTH~Age,ageData)
with(subset(ageData,Age==11),hist(LENGTH,breaks = seq(0,200,5)))
with(subset(ageData,Age==6&YEAR>2009),hist(LENGTH,breaks = seq(0,200,5)))

NAAdata<-ageData |> group_by(YEAR,Age) |> count()
# Plot CAL
ggplot(data=NAAdata,aes(y=YEAR,x=Age,size=n))+ theme_classic() + #ggtitle('Catch PAL') +
  theme(panel.grid.major.y = element_line(color='grey'),legend.position = 'none') + geom_point(colour='red',alpha=0.5) +
  scale_x_continuous('Age',limits=c(min(NAAdata$Age)-1,max(NAAdata$Age)+1), expand = c(0,0)) +
  scale_y_reverse(name='Year',limits=c(min(NAAdata$YEAR)-1,max(NAAdata$YEAR)+1),expand=c(0,0)) +
  geom_vline(xintercept = max(NAAdata$Age)+1) + geom_hline(yintercept = min(NAAdata$YEAR)-1)


######## DB source data for Andrea ###############


#use Mike's package, install if necessary
#library(devtools)
#install_github("Maritimes/Mar.datawrangling")
library(Mar.datawrangling)


# read in age data provided
agedata = read.csv(file.path(datadir,"AgeData","ageData.csv"))
NSRVtrips = unique(subset(agedata,dbSOURCE=="NS_RVSurvey")$TRIP)
ISDBtrips =unique(subset(agedata,dbSOURCE=="ISDB")$TRIP)
GPStrips = unique(subset(agedata,dbSOURCE=="GPS")$TRIP)

#RV data
# extract data using RORACLE (RODBC is also an option), Make sure data directory exists
#get_data("rv", data.dir = file.path(getwd(), "data"), usepkg = "roracle", fn.oracle.username = uid, fn.oracle.password=pwd, fn.oracle.dsn = "PTRAN", force.extract = T)

get_data("rv", data.dir = datadir)

# filter for halibut and the missions represented in the age data
GSSPECIES = GSSPECIES[GSSPECIES$CODE==30,]
GSMISSIONS = GSMISSIONS[GSMISSIONS$MISSION%in%NSRVtrips,]
self_filter("rv")

NSRVdata=merge(select(GSDET, MISSION, SETNO, FSHNO, FLEN, FSEX, AGE),select(GSINF, MISSION, SETNO, SDATE, LATITUDE, LONGITUDE))

write.csv(NSRVdata,file.path(datadir,"AgeData","NSRVdata.csv"),row.names=F)

# ISDB
get_data(db='isdb',data.dir=datadir)


# filter for halibut longline survey, halibut and the missions represented in the age data
#ISSPECIESCODES = ISSPECIESCODES[ISSPECIESCODES$CODE==30,]
ISCATCHES = ISCATCHES[ISCATCHES$SPECCD_ID==30,]
ISTRIPS = ISTRIPS[ISTRIPS$TRIP%in%ISDBtrips,]
self_filter("isdb")

tripinfo = left_join(select(ISTRIPS,TRIP_ID,TRIP),select(ISFISHSETS,FISHSET_ID,TRIP_ID))

setinfo = left_join(select(ISCATCHES, CATCH_ID, FISHSET_ID),select(ISSETPROFILE, FISHSET_ID, SET_NO, DATE_TIME4, LATITUDE, LONGITUDE, YEAR))

Mar.datawrangling::get_data_custom(schema="observer", data.dir = datadir, tables = c("ISFISHLENGTHS","ISSAMPLES"))

#The new tables get downloaded and/or loaded in and you can filter them manually
ISSAMPLES = subset(ISSAMPLES,CATCH_ID %in% ISCATCHES$CATCH_ID,c("SMPL_ID","CATCH_ID","SEXCD_ID"))
ISFISHLENGTHS=subset(ISFISHLENGTHS,SMPL_ID %in% ISSAMPLES$SMPL_ID,c("SMPL_ID","FISH_LENGTH","NUM_AT_LENGTH"))

fishlengths <- left_join(ISSAMPLES,ISFISHLENGTHS)


ISDBdata = left_join(tripinfo,setinfo) %>%
  right_join(fishlengths)

#ISDBdata = left_join(tripinfo,setinfo) %>% right_join(select(ISFISH, CATCH_ID, FISH_NO, FISH_LENGTH, SEXCD_ID, OTOLITH_COLLECTED)) %>%

write.csv(ISDBdata,file.path(datadir,"AgeData","ISDBdata.csv"),row.names=F)

# Port Sampling
get_data_custom('mfd_port_samples', tables=c('GPSAMPLES', 'GPLENGTHS','GPMARKETS','GPUNIQ_AREA2'), data.dir =datadir, fn.oracle.username = uid, fn.oracle.password = pwd, fn.oracle.dsn = 'ptran',usepkg='roracle')

GPSdata = left_join(subset(GPSAMPLES,TRIP_NUMBER%in%GPStrips,c("SAMPLE","AREA","TRIP_NUMBER")),GPUNIQ_AREA2[,c("AREACODE","DESCRIPTION")],by = c("AREA" = "AREACODE")) %>%
  left_join(select(GPLENGTHS, SAMPLE, SEX, LENGROUP, NUMATLEN))

write.csv(GPSdata,file.path(datadir,"AgeData","GPSdata.csv"),row.names=F)


#

#
# GPSdata = left_join(tripinfo,setinfo) %>%
#   right_join(select(ISFISH, CATCH_ID, FISH_NO, FISH_LENGTH, SEXCD_ID, OTOLITH_COLLECTED))
#
#
#
# get_data("gps", data.dir = file.path(getwd(), "data"), usepkg = "roracle", fn.oracle.username = uid, fn.oracle.password=pwd, fn.oracle.dsn = "PTRAN")
# load(file.path(datadir,"mfd_port_samples.gplengths.rdata"))
# load(file.path(datadir,"mfd_port_samples.gpsamples.rdata"))
#
#
#
# # replicate the bins in the age data and compare histograms
# bins=seq(2,200,3)
# x=hist(GSDET$FLEN,breaks=bins)
# y=hist(agedata$FLEN,breaks=bins)
# BINS = paste0("(",bins[-length(bins)],",",bins[-1],"]")
#
# data.frame(BINS,N_FISH_CAUGHT=x$counts,N_FISH_AGED=y$counts)
#
#
#
#
# HalibutAgeDataRVsurvey = subset(ageData,dbSOURCE=="NS_RVSurvey",c("TRIP","FISH_NO","Age"))
#
# write.csv(HalibutAgeDataRVsurvey,file.path(datadir,"AgeData","HalibutAgeDataRVsurvey.csv"),row.names=F)

###### take 2 for older data Armsworthy & Campana


# read in age data provided
agedata2 = read.csv(file.path(datadir,"AgeData","AC_data","AC2010.csv"))
unique(agedata2$source)
NSRVtrips2 = unique(subset(agedata2,source=="SF RV")$trip)
ISDBtrips2 =unique(subset(agedata2,source=="ISDB")$trip)
GPStrips2 = unique(subset(agedata2,source%in%c("GPS","ISDB&GPS"))$trip)
NLtrips2 = unique(subset(agedata2,source%in%c("Nfld RV","NFLD IOP"))$trip)

get_data("rv", data.dir = datadir)

# filter for halibut and the missions represented in the age data
GSSPECIES = GSSPECIES[GSSPECIES$CODE==30,]
GSMISSIONS = GSMISSIONS[GSMISSIONS$MISSION%in%NSRVtrips2,]
self_filter("rv")

NSRVdata2=merge(select(GSDET, MISSION, SETNO, FSHNO, FLEN, FSEX, AGE),select(GSINF, MISSION, SETNO, SDATE, LATITUDE, LONGITUDE))

write.csv(NSRVdata2,file.path(datadir,"AgeData","NSRVdata2.csv"),row.names=F)


# ISDB
get_data(db='isdb',data.dir=datadir)


# filter for halibut longline survey, halibut and the missions represented in the age data
#ISSPECIESCODES = ISSPECIESCODES[ISSPECIESCODES$CODE==30,]
ISTRIPS = ISTRIPS[ISTRIPS$TRIP%in%ISDBtrips2,]
ISCATCHES = ISCATCHES[ISCATCHES$SPECCD_ID==30,]
self_filter("isdb")

tripinfo = left_join(select(ISTRIPS,TRIP_ID,TRIP),select(ISFISHSETS,FISHSET_ID,TRIP_ID))

setinfo = left_join(select(ISCATCHES, CATCH_ID, FISHSET_ID, SPECCD_ID ),select(ISSETPROFILE, FISHSET_ID, SET_NO, DATE_TIME4, LATITUDE, LONGITUDE, YEAR))

Mar.datawrangling::get_data_custom(schema="observer", data.dir = datadir, tables = c("ISFISHLENGTHS","ISSAMPLES"))

#The new tables get downloaded and/or loaded in and you can filter them manually
ISSAMPLES = subset(ISSAMPLES,CATCH_ID %in% ISCATCHES$CATCH_ID,c("SMPL_ID","CATCH_ID","SEXCD_ID"))
ISFISHLENGTHS=subset(ISFISHLENGTHS,SMPL_ID %in% ISSAMPLES$SMPL_ID,c("SMPL_ID","FISH_LENGTH","NUM_AT_LENGTH"))

fishlengths <- left_join(ISSAMPLES,ISFISHLENGTHS)


ISDBdata2 = left_join(tripinfo,setinfo) %>%
  right_join(fishlengths)

#ISDBdata = left_join(tripinfo,setinfo) %>% right_join(select(ISFISH, CATCH_ID, FISH_NO, FISH_LENGTH, SEXCD_ID, OTOLITH_COLLECTED)) %>%


write.csv(ISDBdata2,file.path(datadir,"AgeData","ISDBdata2.csv"),row.names=F)


# Port Sampling
get_data_custom('mfd_port_samples', tables=c('GPSAMPLES', 'GPLENGTHS','GPMARKETS','GPUNIQ_AREA2'), data.dir =datadir, fn.oracle.username = uid, fn.oracle.password = pwd, fn.oracle.dsn = 'ptran',usepkg='roracle')

GPSdata2 = left_join(subset(GPSAMPLES,TRIP_NUMBER%in%GPStrips2,c("SAMPLE","AREA","TRIP_NUMBER")),GPUNIQ_AREA2[,c("AREACODE","DESCRIPTION")],by = c("AREA" = "AREACODE")) %>%
  left_join(select(GPLENGTHS, SAMPLE, SEX, LENGROUP, NUMATLEN))

write.csv(GPSdata2,file.path(datadir,"AgeData","GPSdata2.csv"),row.names=F)

# NL data
#NLdata <-




####### 2026 ##### Age Data Summary ########

source(file.path(getwd(), "directories.r"))
source(file.path(wd, "passwords.r"))
library(tidyverse)

AgeSummary<-read.csv(file.path(datadir,"AgeData","All_Halibut_Ages_updatedNov2025_YRsummary.csv"))
AgeData<-read.csv(file.path(datadir,"AgeData","All_Halibut_Ages_updatedNov2025.csv"))
isdbOtoliths<-read.csv(file.path(datadir,"AgeData","ISDB_availableotoliths2025.csv"))
rvOtoliths<-read.csv(file.path(datadir,"AgeData","MARRV_availableotoliths2025.csv"))

Ages <- AgeData |>
  mutate(SOURCE = ifelse(FISH_SOURCE %in% c("GPS","ISDB","ISDB&GPS","NFLD IOP"), "ISDB", "RV")) |>
  mutate( NAFO=replace(NAFO, NAFO%in%c("4X","4XM","4XN","4XO","4XP","4XQ","4XR","4XS" ,"5Y","5YB","5Ze","5ZJ"), "4X"))  |>
  mutate( NAFO=replace(NAFO, NAFO%in%c("3P,4V","4V","4VB","4VC","4Vn", "4VN","4Vs","4VS","4VsW","4VW","4W","4W,4X", "4WG","4WJ" ,"4WL","4WM"), "4VW"))  |>
  mutate( NAFO=replace(NAFO, NAFO%in%c("3N","3NB","3NC","3ND","3NE","3NF","3O","3OA","3OC", "3OD","3OE","3P","3PS"), "3NOP"))  |>
  filter(NAFO%in%c("4X","4VW","3NOP"))

AgeSummary <- Ages |>
  group_by(SOURCE,YEAR,NAFO) |>
  summarise(n = n())|>
  pivot_wider(names_from = NAFO, values_from = n)

write.csv(AgeSummary,"AgeSummary.csv")

Otoliths <- rbind(isdbOtoliths,rvOtoliths) |>
  mutate( NAFO=replace(NAFO, NAFO%in%c("4X","5Y","5Z"), "4X"))  |>
  mutate( NAFO=replace(NAFO, NAFO%in%c("4V","4VN","4VS","4VSb", "4VSc", "4VSW", "4W" ,"4Wgj", "4Wlx"), "4VW"))  |>
  mutate( NAFO=replace(NAFO, NAFO%in%c("3N","3O","3P"), "3NOP"))  |>
  filter(NAFO%in%c("4X","4VW","3NOP"))

OtolithsSummary <-  Otoliths |>
  group_by(SOURCE,YEAR,NAFO) |>
  summarise(n = n()) |>
  pivot_wider(names_from = NAFO, values_from = n)

write.csv(OtolithsSummary,"OtolithsSummary.csv")


AgeData$SEX[is.na(AgeData$SEX)]<-0
sx=c(1,2,0)
yrs<-1970:2025
bins=seq(5,260,5)
LF_Ages_isdb<-list()
LF_Ages_rv<-list()
LF_Otoliths_isdb<-list()
LF_Otoliths_rv<-list()
for(i in 1:length(yrs)){
  LF_Ages_isdb[[i]]<-t(sapply(sx,function(s){with(subset(Ages,YEAR==yrs[i]&SEX==s&SOURCE=="ISDB"),hist(LENGTH,breaks=bins,plot=F,right=F)$counts)}))
  LF_Ages_rv[[i]]<-t(sapply(sx,function(s){with(subset(Ages,YEAR==yrs[i]&SEX==s&SOURCE=="RV"),hist(LENGTH,breaks=bins,plot=F,right=F)$counts)}))
  LF_Otoliths_isdb[[i]]<-t(sapply(sx,function(s){with(subset(Otoliths,YEAR==yrs[i]&SEX==s&SOURCE=="ISDB"),hist(LENGTH,breaks=bins,plot=F,right=F)$counts)}))
  LF_Otoliths_rv[[i]]<-t(sapply(sx,function(s){with(subset(Otoliths,YEAR==yrs[i]&SEX==s&SOURCE=="RV"),hist(LENGTH,breaks=bins,plot=F,right=F)$counts)}))

}
names(LF_Ages_isdb)<-yrs
names(LF_Ages_rv)<-yrs
names(LF_Otoliths_isdb)<-yrs
names(LF_Otoliths_rv)<-yrs

pltYrs1<-1970:1999
pltYrs2<-2000:2025

BarPlotLF(LF_Ages_rv[which(names(LF_Ages_rv)%in%pltYrs1)],yrs=pltYrs1,rel=F,rows=15,filen=file.path(wd,"figures","AgesRV1"),graphic='png',ax=c(rep(2,15),rep(4,15)),ylp=0.6,add.sample.size = T,ymax=40)

BarPlotLF(LF_Ages_rv[which(names(LF_Ages_rv)%in%pltYrs2)],yrs=pltYrs2,rel=F,rows=15,filen=file.path(wd,"figures","AgesRV2"),graphic='png',ax=c(rep(2,15),rep(4,15)),ylp=0.6,add.sample.size = T,ymax=40)

BarPlotLF(LF_Otoliths_rv[which(names(LF_Otoliths_rv)%in%pltYrs1)],yrs=pltYrs1,rel=F,rows=15,filen=file.path(wd,"figures","OtolithsRV1"),graphic='png',ax=c(rep(2,15),rep(4,15)),ylp=0.6,add.sample.size = T,ymax=40)

BarPlotLF(LF_Otoliths_rv[which(names(LF_Otoliths_rv)%in%pltYrs2)],yrs=pltYrs2,rel=F,rows=15,filen=file.path(wd,"figures","OtolithsRV2"),graphic='png',ax=c(rep(2,15),rep(4,15)),ylp=0.6,add.sample.size = T,ymax=40)

BarPlotLF(LF_Ages_isdb[which(names(LF_Ages_isdb)%in%pltYrs2)],yrs=pltYrs2,rel=F,rows=15,filen=file.path(wd,"figures","AgesISBD"),graphic='png',ax=c(rep(2,15),rep(4,15)),ylp=0.1,add.sample.size = T,ymax=60)

BarPlotLF(LF_Otoliths_isdb[which(names(LF_Otoliths_isdb)%in%pltYrs2)],yrs=pltYrs2,rel=F,rows=15,filen=file.path(wd,"figures","OtolithsISBD"),graphic='png',ax=c(rep(2,15),rep(4,15)),ylp=0.1,add.sample.size = T,ymax=500)

