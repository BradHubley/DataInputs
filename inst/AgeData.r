
ageData<-read.csv(file.path(data.dir,"Ageing","ageData.csv"))
head(ageData)
summary(ageData)
plot(Age~LENGTH,ageData)
with(subset(ageData,Age==11),hist(LENGTH,breaks = seq(0,200,5)))
with(subset(ageData,Age==6&YEAR>2009),hist(LENGTH,breaks = seq(0,200,5)))


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

setinfo = left_join(select(ISCATCHES, CATCH_ID, FISHSET_ID),select(ISSETPROFILE_WIDE, FISHSET_ID, SET_NO, DATE_TIME4, LATITUDE, LONGITUDE, YEAR))

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

setinfo = left_join(select(ISCATCHES, CATCH_ID, FISHSET_ID, SPECCD_ID ),select(ISSETPROFILE_WIDE, FISHSET_ID, SET_NO, DATE_TIME4, LATITUDE, LONGITUDE, YEAR))

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
NLdata <-
