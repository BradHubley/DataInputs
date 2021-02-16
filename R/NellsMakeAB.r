#Nell's ab function
make.ab<-function(dat, wd){
#take data, drop outliers, convert area 5 to 4 to check for difference between 4WVX5Z and 3NOP
#do not model difference between areas, and sample size is by sex and year, all areas
#get a and b from linear mixed effects, log transformed length and weight, random year effect

dat<-dat[dat$out==1,]
dat$area<-substr(dat$nafo,1,1)
dat$area[dat$area==5]<-4
dat$ll<-log10(dat$len)
dat$lw<-log10(dat$wt)
dat.male<-dat[dat$sex%in%c(1),]
dat.female<-dat[dat$sex%in%c(2),]
dat.unk<-dat

##Unknown  ---- is all data, male, female and unknown
wt.dat<-dat
require(nlme)
random.effect="year"
b.par='estimate'
verbose=T
GBmodel=F

	require(nlme)
	wt.dat$raneff<-wt.dat[,random.effect]
	ran.effects<-unique(wt.dat$raneff)

	wt.gdat <- groupedData(wt ~ len | raneff, data = wt.dat)
	a <- c()
	b <- c()

		wt.lme1 <- lme(fixed = log(wt) ~ log(len)+as.factor(area), data = wt.gdat, random = ~ 1 | raneff, method="REML")
		
	summary(wt.lme1)  ##area is not significant
	
	 wt.lme <- lme(fixed = log(wt) ~ log(len), data = wt.gdat, random = ~ 1 | raneff, method="REML")   ##defined the random effect as only change in the intercept #ndh Aug 1, 2014
	
		if(is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=row.names(coef(wt.lme)),coef(wt.lme))
		if(!is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=sort(row.names(coef(wt.lme))),a=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),1],b=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),2])
		for(i in 1:length(ran.effects)){
			a[i] <- exp(fit[i,2])
			b[i] <- fit[i,3]
		}
		A <- exp(wt.lme$coef$fixed[1])  ###exponentiated this to make easier for me #ndh Aug 1, 2014
		B <- wt.lme$coef$fixed[2]
		if(GBmodel){
			a <- c(exp(as.numeric(wt.lme$coef$fixed[1])),a)
			b <- c(as.numeric(wt.lme$coef$fixed[2]),b)
}
unk.a<-a
unk.b<-b			
unk.A<-A
unk.B<-B
	
##male
wt.dat<-dat[dat$sex==1,]
	wt.dat$raneff<-wt.dat[,random.effect]
	ran.effects<-unique(wt.dat$raneff)

	wt.gdat <- groupedData(wt ~ len | raneff, data = wt.dat)
	a <- c()
	b <- c()

		wt.lme <- lme(fixed = log(wt) ~ log(len), data = wt.gdat, random = ~ 1 | raneff, method="REML")
		
	summary(wt.lme)
		
		if(is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=row.names(coef(wt.lme)),coef(wt.lme))
		if(!is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=sort(row.names(coef(wt.lme))),a=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),1],b=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),2])
		for(i in 1:length(ran.effects)){
			a[i] <- exp(fit[i,2])
			b[i] <- fit[i,3]
		}
		A <- exp(wt.lme$coef$fixed[1])
		B <- wt.lme$coef$fixed[2]
		if(GBmodel){
			a <- c(exp(as.numeric(wt.lme$coef$fixed[1])),a)
			b <- c(as.numeric(wt.lme$coef$fixed[2]),b)
}
male.a<-a
male.b<-b			
male.A<-A
male.B<-B

#female
wt.dat<-dat[dat$sex==2,]
	require(nlme)
	wt.dat$raneff<-wt.dat[,random.effect]
	ran.effects<-unique(wt.dat$raneff)

	wt.gdat <- groupedData(wt ~ len | raneff, data = wt.dat)
	a <- c()
	b <- c()

		wt.lme <- lme(fixed = log(wt) ~ log(len), data = wt.gdat, random = ~ 1 | raneff, method="REML")
		
		summary(wt.lme)
		
			if(is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=row.names(coef(wt.lme)),coef(wt.lme))
		if(!is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=sort(row.names(coef(wt.lme))),a=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),1],b=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),2])
		for(i in 1:length(ran.effects)){
			a[i] <- exp(fit[i,2])
			b[i] <- fit[i,3]
		}
		A <- exp(wt.lme$coef$fixed[1])
		B <- wt.lme$coef$fixed[2]
		if(GBmodel){
			a <- c(exp(as.numeric(wt.lme$coef$fixed[1])),a)
			b <- c(as.numeric(wt.lme$coef$fixed[2]),b)
			}

female.a<-a
female.b<-b			
female.A<-A
female.B<-B

##number of observations in that year of that sex in any area
x<-as.data.frame(with(dat, table(year, sex)))
male.n<-x$Freq[x$sex==1]
female.n<-x$Freq[x$sex==2]
unk.n<-male.n+female.n+x$Freq[x$sex==0]


abdat2<-NULL
##OK generate hal len wt
YEAR<-as.character(rep(1970:2013, 6))
SEX<-as.character(c(rep(1, length(1970:2013)*2), rep(2, length(1970:2013)*2), rep(0, length(1970:2013)*2)))
NAFO_AREA<-rep(c((rep("3N,3O,3P", length(1970:2013))), (rep("4V,4W,4X,5Z", length(1970:2013)))), 3)
NOTES<-rep("LME model 1970-2013 by sex from make.ab. 2014", length(YEAR)) 
LEN_WT_B<-as.character(round(c(male.b, male.b,  female.b, female.b,  unk.b, unk.b ),5))
LEN_WT_A<-as.character(round(c(male.a, male.a,  female.a, female.a,  unk.a, unk.a ),5))
LEN_WT_N<-as.character(c(male.n, male.n,  female.n, female.n,  unk.n, unk.n ))

abdat2<-data.frame(cbind(YEAR,SEX,NAFO_AREA,LEN_WT_N,LEN_WT_A, LEN_WT_B,NOTES))

return(abdat2)

}
