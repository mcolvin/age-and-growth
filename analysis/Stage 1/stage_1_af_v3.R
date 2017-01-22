# MEAN AGE: STAGE 2	
require(plyr)
require(reshape2)
library(FSA)
setwd("C:/Users/mcolvin/Documents/projects/Age and Growth/Analysis/Stage 1")

fish<- data.frame(linf=seq(20,100,10))

nsims=1000

# MEAN AGE: STAGE 1
out<- data.frame()
for(kk in c(1:nrow(fish)))
	{
	N<- 100000
	Linf<- fish$linf[kk]
	k<- exp(1.2434+-0.66*log(Linf))
	t0<- 0	
	maxAge<-round(log(1-(((Linf*0.95)/Linf)))/-k + t0)
	spp<- "generic"
	species<- "generic"
	b = -1.01
	a = exp(1.46)
	A<- 1-exp(-a*maxAge^b )
	A_f<- "none"
	survival<- cumprod(rep(1-A,maxAge))
	dat<-data.frame(age=c(1:maxAge),N=rmultinom(1,N,survival))
	dat$exp_length<- Linf  * (1 - exp(-k * (dat$age-t0))) *10 # in mm  
	pop<- as.data.frame(lapply(dat,function(x) rep(x,dat$N)))
	pop$age_f<- as.factor(pop$age)
	# ASSIGN LENGTH TO EXPANDED DATASET
	pop$len_true<- rnorm(length(pop$exp_length),pop$exp_length,pop$exp_length*0.2)
	pop$len_obs<- round(pop$len_true,1)
	# age frequency
	af<-dcast(pop,age~"af_pop",value.var="len_obs",length)
	af$af_pop<- af$af_pop/sum(af$af_pop)
	ss<- c(seq(30,100,10),
		seq(125,1000, by=25),
		seq(1100,10000,by=100),
		seq(11000,25000,by=1000))
	ss<- ss[ss<5000]

	for(i in c(1:length(ss)))
		{
		for(x in c(1:nsims))
			{
			srs_indx<- sample(c(1:nrow(pop)),ss[i],replace=FALSE)
			srs<-dcast(pop[srs_indx,],age~"af_srs",value.var="len_obs",length)
			srs$af_srs<- srs$af_srs/sum(srs$af_srs)
			xx<- merge(af,srs,by="age",all.x=TRUE)
			#srs_se= c(sqrt(srs*(1-srs)/table(pop[srs_indx,]$age_f)))
			#srs<- data.frame(p=srs,se=srs_se)
			out<- data.frame(
				species=kk,
				spp=spp,
				A=A,
				A_f=A_f,
				ss=ss[i],
				sim=x,
				age=xx$age,
				mla_age_true=xx$af_pop,
				mla_age_srs=xx$af_srs)
			if(kk==1 & i==1 & x==1)
				{write.csv(out,"./output/stage_1_af.csv")} else
				{write.table(out,"./output/stage_1_af.csv",
					append=TRUE, col.names=FALSE,sep=',')}
			}
		}	
	}

