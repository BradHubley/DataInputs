library(SpatialHub)

spm<-subset(EEZ,PID==1)
spm$POS<-1:nrow(spm)

load(file.path(datadir,"Survey","SurveyStrata2022.rdata"))

spmStrata<-joinPolys(subset(surveyStrataPolyLL,PID%in%c(33,41:43)),spm,operation="INT")

attr(spmStrata,"projection")<-"LL"

spmAreas<-calcArea(spmStrata,1)

write.csv(spmAreas,file.path(datadir,"Survey","SPMareas.csv"),row.names = F)
