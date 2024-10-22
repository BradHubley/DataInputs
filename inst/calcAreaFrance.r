library(SpatialHub)

spm<-subset(EEZ,PID==1)
spm$POS<-1:nrow(spm)

load(file.path(datadir,"Survey","SurveyStrata2022.rdata"))

spmStrata<-joinPolys(subset(surveyStrataPolyLL,PID%in%c(33,41:43)),spm,operation="INT")

attr(spmStrata,"projection")<-"LL"

spmAreas<-calcArea(spmStrata,1)

write.csv(spmAreas,file.path(datadir,"Survey","SPMareas.csv"),row.names = F)




############## NRA

nra<-clipPolys(subset(EEZ,PID==2),xlim=c(-55,-45),ylim=c(42,47))
nraStrata<-joinPolys(subset(surveyStrataPolyLL,PID%in%51:53),nra,operation="DIFF")

attr(nraStrata,"projection")<-"LL"

nraAreas<-calcArea(nraStrata,1)

write.csv(nraAreas,file.path(datadir,"Survey","NRAareas.csv"),row.names = F)



NAFO <- sf::st_read(file.path(datadir,"Mapping","NAFODivisions","Divisions.shp"))
st_geometry(NAFO)


