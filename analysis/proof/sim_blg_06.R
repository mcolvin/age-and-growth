
library(plyr)
library(reshape2)
library(FSA)
library(snowfall)
library(rlecuyer)


setwd("C:/Users/mcolvin/Documents/projects/Age and Growth/Analysis/proof")
N<- 500000
A<- 0.6
maxAge<- 6
survival<- cumprod(rep(1-A,maxAge))
dat<-data.frame(age=c(1:maxAge),N=rmultinom(1,N,survival))

# BLUEGILL
Linf<- 21.5
k<- 0.45
t0<- -0.313


dat$exp_length<- Linf  * (1 - exp(-k * (dat$age-t0))) *10 # in mm  
pop<- as.data.frame(lapply(dat,function(x) rep(x,dat$N)))
pop$age_f<- as.factor(pop$age)
# ASSIGN LENGTH TO EXPANDED DATASET
pop$len_true<- rnorm(length(pop$exp_length),pop$exp_length,pop$exp_length*0.2)
pop$len_obs<- round(pop$len_true,1)


out<-len_structure<-age_structure<-mn_len_at_age<-vbgf_out<- data.frame()

bin<-c(10,25)
bin<- 10
for(i in 1:8)#length(bin))
{
  bins<- seq(0,(max(pop$len_obs)+100),bin[i])
  bin_func<- approxfun(bins,bins, method="constant")
  pop$l_bin<- bin_func(pop$len_obs)
  pop$l_bin<-factor(pop$l_bin,levels=bins)
  
  # POPULATION LEVEL MEAN
  out<- rbind(out,data.frame(ss=0,bin=bin[i],bin_n=0,sim=0,type="pop",
                             length=mean(pop$len_obs),age=mean(pop$age)))
  
  # POPULATION LEVEL: LENGHT AND AGE STRUCTURE
  len_structure<- data.frame(ss=0,bin=bin[i],bin_n=0,sim=0,level="pop",l_bin=bins,
                             rel_freq=c(prop.table(table(pop$l_bin))))
  age_structure<- data.frame(ss=0,bin=bin[i],bin_n=0,sim=0,level="pop",age=unique(pop$age),
                             rel_freq=c(prop.table(table(pop$age))))
  
  # POPULATION LEVEL: MEAN LENGTH AT AGE
  mn_len_at_age<- aggregate(len_obs~age,pop,mean)
  mn_len_at_age$level<-"pop"
  mn_len_at_age$sim<-0
  mn_len_at_age$bin<- bin[i]
}
	j=0
	fn<- paste("./output/blg_means_",j,".csv",sep="")
	write.csv(out,fn)
	fn<- paste("./output/blg_mean_len_at_age",j,".csv",sep="")	
	write.csv(mn_len_at_age,fn)
	fn<- paste("./output/blg_len_structure_",j,".csv",sep="")	
	write.csv(len_structure,fn)
	fn<- paste("./output/blg_age_structure_",j,".csv",sep="")	
	write.csv(age_structure,fn)


# START SAMPLE SIZE EVALUATION
sampleSize<-expand.grid(ss= c(seq(100,500,100),750,1000,1500,2000),
	bin=c(1,2.5),
	bin_n=c(2:10,12,14,16,18,20,25,30),
	i=c(1:500))

	# SET UP FOR MULTICORE
	ncpus=4 # number of cores to use
	sfInit(parallel=T, cpus=ncpus)
	sfExportAll()
	sfLibrary(reshape2)
	sfLibrary(plyr)
	sfLibrary(FSA)
	sfClusterSetupRNG()
	# RUN THE FUNCTION run_mc on 4 cores
	result <- sfLapply(1:nrow(sampleSize), run)
	sfStop()

	
	  
	
	
run<- function(j)
	{
	vbgf_out<- data.frame()
	ss<- sampleSize$ss[j]
	bin<- sampleSize$bin[j]*10
	bins<- seq(0,(max(pop$len_obs)+100),bin)
	bin_n<- sampleSize$bin_n[j]
	bin_func<- approxfun(bins,bins, method="constant")
	pop$l_bin<- bin_func(pop$len_obs)
	pop$l_bin<-factor(pop$l_bin,levels=bins)
	i<- sampleSize$i[j]

	## SRS
	srs_indx<- sample(c(1:nrow(pop)),ss,replace=FALSE)
	srs<- pop[srs_indx,]
	srs$bin_indx<- 1
	srs<- srs[order(srs$l_bin),]
	x<-aggregate(bin_indx~l_bin,srs,sum)
	srs$bin_indx<- unlist(sapply(1:nrow(x),
		function(i){return(1:x$bin_indx[i])}))

	### SRS LEVEL MEAN
	out<-data.frame(ss=ss,bin=bin,bin_n=bin_n,sim=i,type="srs",
								length=mean(srs$len_obs),age=mean(srs$age))

	### SRS: LENGTH AND AGE STRUCTURE
	srs_len_structure<- prop.table(table(srs$l_bin))
	srs_age_structure<- prop.table(table(srs$age))

	len_structure<-   data.frame(ss=ss,bin=bin,bin_n=bin_n,sim=i,level="srs",l_bin=bins,
									 rel_freq=c(prop.table(table(srs$l_bin))))
	yyy	<- 	c(prop.table(table(srs$age)))					 
	age_structure<- data.frame(ss=ss,bin=bin,bin_n=bin_n,sim=i,level="srs",
									 age=as.numeric(names(yyy)),
									 rel_freq=yyy) 
	### SRS: MEAN LENGTH AT AGE
	xx<- aggregate(len_obs~age,srs,mean)
	xx$level<-"SRS"
	xx$sim<-i
	xx$bin<-bin
	mn_len_at_age<- xx

	## ALK 	
	alk<- subset(srs,bin_indx<=bin_n)
	### MEAN LENGTH AT AGE (FSA)
	len.n <- xtabs(~l_bin,data=srs)
	raw <- xtabs(~l_bin+age_f,data=alk)
	key <- prop.table(raw, margin=1)
	xx<- suppressWarnings(alkMeanVar(key,len_obs~l_bin+age_f,alk,len.n)[,c(1:2)])
	xx$level<-"ALK"
	xx$sim<-i
	xx$bin<-bin
	names(xx)<- names(mn_len_at_age)
	mn_len_at_age<- rbind(mn_len_at_age,xx)

	### ALK LEVEL MEAN
	key[is.na(key)]<-0
	srs_age_struc_hat<- prop.table(t(key) %*% table(srs$l_bin))
	srs_age_struc_hat<-data.frame(age=as.numeric(rownames(srs_age_struc_hat)),  struc=srs_age_struc_hat)
	srs_mean_age_hat<- sum(srs_age_struc_hat*as.numeric(rownames(srs_age_struc_hat)))
	
	out<-rbind(out,data.frame(ss=nrow(alk),bin=bin,bin_n=bin_n,sim=i,type="alk",
								length=nrow(alk),age=srs_mean_age_hat))
	age_structure<- rbind(age_structure,data.frame(ss=ss,bin=bin,bin_n=bin_n,sim=i,level="alk",
			age=srs_age_struc_hat$age,
			rel_freq=srs_age_struc_hat$struc))
			
	# VBGF
	
	## SRS LEVEL VBGF
	fit<- try(
		nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
		srs,
		start=list(
			Linf=Linf*10, 
			k=k, 
			t0=t0),
		control=list(maxiter=3000)),
		silent=T)
	if(class(fit)=="try-error") {y<- data.frame(est=rep(-99,3),se=rep(-99,3));rownames(y)<-c("Linf","k","t0")	}
	if(class(fit)!="try-error") {y<-summary(fit)$coefficients[,c(1:2)]	}		

	vbgf_out<- rbind(vbgf_out,
		data.frame(ss=nrow(alk),bin=bin,bin_n=bin_n,sim=i,type="srs",
								linf = y[1,1],
								k= y[2,1],
								t0= y[3,1],
								se_linf= y[1,2],
								se_k= y[2,2],
								se_t0= y[3,2])) 
	
	## MEAN LENGTH AT AGE, UNWEIGHTED
	xx<- suppressWarnings(
		alkMeanVar(key,len_obs~l_bin+age_f,
			alk,len.n))
	fit<- try(
		nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
		mn_len_at_age,
		start=list(
			Linf=Linf*10, 
			k=k, 
			t0=t0),
		control=list(maxiter=3000)),
		silent=T)
	if(class(fit)=="try-error") {y<- data.frame(est=rep(-99,3),se=rep(-99,3));rownames(y)<-c("Linf","k","t0")	}
	if(class(fit)!="try-error") {y<-summary(fit)$coefficients[,c(1:2)]	}		
	vbgf_out<- rbind(vbgf_out,
		data.frame(ss=nrow(alk),bin=bin,bin_n=bin_n,sim=i,type="mn_l_age_uw",
								linf = y[1,1],
								k= y[2,1],
								t0= y[3,1],
								se_linf= y[1,2],
								se_k= y[2,2],
								se_t0= y[3,2])) 

		
	## MEAN LENGTH AT AGE, WEIGHTED
	fit<- try(
		nls(mean~ Linf * (1- exp(-k*(age-t0))),
		xx,start=list(
			Linf=Linf*10, 
			k=k, 
			t0=t0),
		weights= (xx$sd^2)^-1,
		control=list(maxiter=3000)),
		silent=T)	
	if(class(fit)=="try-error") {y<- data.frame(est=rep(-99,3),se=rep(-99,3));rownames(y)<-c("Linf","k","t0")	}
	if(class(fit)!="try-error") {y<-summary(fit)$coefficients[,c(1:2)]	}		
	vbgf_out<- rbind(vbgf_out,
		data.frame(ss=nrow(alk),bin=bin,bin_n=bin_n,sim=i,type="mn_l_age_w",
								linf = y[1,1],
								k= y[2,1],
								t0= y[3,1],
								se_linf= y[1,2],
								se_k= y[2,2],
								se_t0= y[3,2])) 
								
	## CHI METHOD
	top<-table(srs$l_bin)/nrow(srs)
	bot<-table(alk$l_bin)/nrow(alk)
	rw<- data.frame(l_bin=levels(srs$l_bin),rw= as.numeric(top/bot))
	rw$rw<- ifelse(is.na(rw$rw),1, rw$rw)
	alk<- merge(alk, rw, by="l_bin",all.x=TRUE)
	fit<- try(
		nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
		alk,start=list(
			Linf=Linf*10, 
			k=k, 
			t0=t0),
		weights= alk$rw,
		control=list(maxiter=3000)),
		silent=T)	
	if(class(fit)=="try-error") {y<- data.frame(est=rep(-99,3),se=rep(-99,3));rownames(y)<-c("Linf","k","t0")	}
	if(class(fit)!="try-error") {y<-summary(fit)$coefficients[,c(1:2)]	}		
	vbgf_out<- rbind(vbgf_out,
		data.frame(ss=nrow(alk),bin=bin,bin_n=bin_n,sim=i,type="chi",
								linf = y[1,1],
								k= y[2,1],
								t0= y[3,1],
								se_linf= y[1,2],
								se_k= y[2,2],
								se_t0= y[3,2])) 
	fn<- paste("./output06/blg_means_",j,".csv",sep="")
	write.csv(out,fn)
	fn<- paste("./output06/blg_mean_len_at_age",j,".csv",sep="")	
	write.csv(mn_len_at_age,fn)
	fn<- paste("./output06/blg_len_structure_",j,".csv",sep="")	
	write.csv(len_structure,fn)
	fn<- paste("./output06/blg_age_structure_",j,".csv",sep="")	
	write.csv(age_structure,fn)
	fn<- paste("./output06/blg_vbgf_",j,".csv",sep="")	
	write.csv(vbgf_out,fn)
	}

