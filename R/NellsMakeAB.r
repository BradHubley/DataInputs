#Nell's ab function
make.ab<-function(dat, wd){

#take data, drop outliers, convert area 5 to 4 to check for difference between 4WVX5Z and 3NOP
#do not model difference between areas, and sample size is by sex and year, all areas
#get a and b from linear mixed effects, log transformed length and weight, random year effect

dat<-dat[dat$out==1,] #takes just rows where out = 1
dat$area<-substr(dat$nafo,1,1) #takes the first character of the nafo column and puts it in the area column
dat$area[dat$area==5]<-4 #changes all 5s in the area column to 4s
dat$ll<-log10(dat$len) #logs the length column
dat$lw<-log10(dat$wt) #logs the weight column
dat.male<-dat[dat$sex%in%c(1),] #makes a data frame of just the males (sex code  = 1)
dat.female<-dat[dat$sex%in%c(2),] #makes a data frame of just the females (sex code = 2)
dat.unk<-dat #unknown data frame is just all the data (including male, female, and unknown sex codes)

##Unknown  ---- is all data, male, female and unknown
wt.dat<-dat #creates data frame wt.dat from all the data
require(nlme) #installs/loads the nlme package
random.effect="year" #says that "random.effect" is to be filled in with the year in each function
b.par='estimate' #"b.par" is to be filled with the estimate in each function
verbose=T #
GBmodel=F #

	require(nlme) #pulls in package "nlme"
	wt.dat$raneff<-wt.dat[,random.effect] #creates column called raneff that's the year (because random.effect='year')
	#the blank space in the square brackets before the comma is for rows; leaving blank specifies all rows of column 'random.effect'
	ran.effects<-unique(wt.dat$raneff) #ran.effects is a vector? of all the unique years (since raneff is a column created from the random.effect='year')

	wt.gdat <- groupedData(wt ~ len | raneff, data = wt.dat) #using data frame wt.dat this creates a groupedData class including the data and the formula
	#the groupedData class has the wt.dat data and the formula that has weight against length with the random effect (year)
	#groupedData classes can be used by different modelling and graphing functions using the formula stored right in the class with the data
	a <- c() #saying that 'a' is going to be filled with c()
	b <- c() #saying b will also be filled with c()

		wt.lme1 <- lme(fixed = log(wt) ~ log(len)+as.factor(area), data = wt.gdat, random = ~ 1 | raneff, method="REML")
#above is a linear mixed effects model to predict weight. log of weight and log of length are fixed effects, models as
#factors of area (which is 3 or 4 of NAFO) and then considers the random effect of year (raneff = random.effect='year')

	summary(wt.lme1)  ##area is not significant (based on results in the summary...maybe need to change spatial scale?)

	 wt.lme <- lme(fixed = log(wt) ~ log(len), data = wt.gdat, random = ~ 1 | raneff, method="REML")
##defined the random effect as only change in the intercept #ndh Aug 1, 2014
#different than wt.lme1 as area as a factor is not considered (based on results saying that area has no sig effect)

		if(is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=row.names(coef(wt.lme)),coef(wt.lme))
		if(!is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=sort(row.names(coef(wt.lme))),
		                                                           a=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),1],
		                                                           b=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),2])
		for(i in 1:length(ran.effects)){ #for every unique year (ran.effect<-unique(raneff) and raneff<-random.effect='year')
			a[i] <- exp(fit[i,2]) #'a' is the exponentiated 'X.intercept' from df fit for a given year since column 2 = X.intercept
			b[i] <- fit[i,3] #'b' is the 'log.len' for that given year since column 3 in df fit is log.len?
		}
		A <- exp(wt.lme$coef$fixed[1])  ###exponentiated this to make easier for me #ndh Aug 1, 2014
		#A here is the intercept from the model (average intercept for all years)
		B <- wt.lme$coef$fixed[2]
		#B here is the slope coef (log.len) for the model
		if(GBmodel){
			a <- c(exp(as.numeric(wt.lme$coef$fixed[1])),a) #can this be re-written as a<-c(A,a)
			b <- c(as.numeric(wt.lme$coef$fixed[2]),b)
}
unk.a<-a #"a" or "intercept" values for all years for all data which includes: males, females, unknowns
unk.b<-b #"b" or slope coef for all years (all the same)
unk.A<-A #overall "a" or "intercept" for the model
unk.B<-B #overall "b" or slope coef for the model

##male
wt.dat<-dat[dat$sex==1,] #subsetting for only males (sex code = 1)
	wt.dat$raneff<-wt.dat[,random.effect] #creates raneff column using all the rows of data and the column random.effect
	ran.effects<-unique(wt.dat$raneff) #creates a vector? with all the unique values of year (raneff <- random.effect='year')

	wt.gdat <- groupedData(wt ~ len | raneff, data = wt.dat) #creates groupedData class with the male data and formula of weight against length with a random effect of year
	a <- c() #'a' filled with c()
	b <- c() #'b' also filled with c()

		wt.lme <- lme(fixed = log(wt) ~ log(len), data = wt.gdat, random = ~ 1 | raneff, method="REML")
#creating model with fixed effects of log weight and log length and random effect of year (raneff <- random.effect='year')
	summary(wt.lme) #get the summary of the model above

		if(is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=row.names(coef(wt.lme)),coef(wt.lme))
		if(!is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=sort(row.names(coef(wt.lme))),
		                                                           a=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),1],
		                                                           b=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),2])
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
wt.dat<-dat[dat$sex==2,] #subsetting for the females (sex code = 2)
	require(nlme) #pulls package 'nlme'
	wt.dat$raneff<-wt.dat[,random.effect] #creates column raneff from all the rows of data in column random.effect which is year
	ran.effects<-unique(wt.dat$raneff) #creates a vector? with just all the unique values of random.effect (unique years)

	wt.gdat <- groupedData(wt ~ len | raneff, data = wt.dat) #creates groupedData class with all female data and associated formula
	a <- c() #'a' to be filled with c()
	b <- c() #'b' to be filled with c()

		wt.lme <- lme(fixed = log(wt) ~ log(len), data = wt.gdat, random = ~ 1 | raneff, method="REML")
#linear mixed effects model with log of weight and log of length for female fish are fixed effects and
#there is a random effect of year (raneff <- random.effect = 'year')
		summary(wt.lme) #a summary of the above model

		if(is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=row.names(coef(wt.lme)),coef(wt.lme))
		if(!is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=sort(row.names(coef(wt.lme))),
		                                                           a=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),1],
		                                                           b=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),2])
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
x<-as.data.frame(with(dat, table(year, sex))) #creates a data frame of all the data with columns for year and sex and freq
male.n<-x$Freq[x$sex==1] #creates a vector of the frequencies of males for each year
female.n<-x$Freq[x$sex==2] #creates a vector fo the frequencies of females for each year
unk.n<-male.n+female.n+x$Freq[x$sex==0] #creates a vector of the overall frequencies of halibut for each year (combining unknowns, males, and females)

abdat2<-NULL #generates an empty vector called "abdat2"
##OK generate hal len wt
YEAR<-as.character(rep(1970:2013, 6)) #creates year vector with sequence 1970-2013 repeated 6 times
SEX<-as.character(c(rep(1, length(1970:2013)*2), rep(2, length(1970:2013)*2), rep(0, length(1970:2013)*2)))
#above creates sex vector with '1' repreated the length of '1970-2013' two times, '2' repeated the length or '1970-2013' two times, and '0' repeated the length of '1970-2013' 2 times
NAFO_AREA<-rep(c((rep("3N,3O,3P", length(1970:2013))), (rep("4V,4W,4X,5Z", length(1970:2013)))), 3)
#above creates nafo vector repeating '3N3O3P' the length of 1970-2013', and '4V4W4X5Z the length of 1970-2013, three times (equalling 6 lengths of 1970-2013)
NOTES<-rep("LME model 1970-2013 by sex from make.ab. 2014", length(YEAR))
#creates notes vector the same length as YEAR (6 times 1970-2013)
LEN_WT_B<-as.character(round(c(male.b, male.b,  female.b, female.b,  unk.b, unk.b ),5))
#creates a vector of b values from the models for males, females, and unknowns
  #should have these values for all years? (as year is the random effect...?)
  #not totally sure what the 5 does here...
  #why are the values repeated? (ex: male.b, male.b...)
LEN_WT_A<-as.character(round(c(male.a, male.a,  female.a, female.a,  unk.a, unk.a ),5))
#creates a vector of a values from models for males, females, and unknowns
  #same issues of confusion as above...?
LEN_WT_N<-as.character(c(male.n, male.n,  female.n, female.n,  unk.n, unk.n ))
#creates a vector of the frequencies of males, females, and unknown by year?
abdat2<-data.frame(cbind(YEAR,SEX,NAFO_AREA,LEN_WT_N,LEN_WT_A, LEN_WT_B,NOTES))
#binds all the vectors above into a data.frame that gives you the a's and b's for every year, by sex and nafo, for the dataset
return(abdat2)

}
