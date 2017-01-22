

source("C:/Users/mcolvin/Documents/projects/Age and Growth/Analysis/global/global_functions.R")




# ANALYSIS FUNCTION
analysis<- function(i,cv_vbgf,bin_int,bin_n,ss,linf,k,t0)
	{
	# EXPAND CATCH COUNT DATA	
	catch<- as.data.frame(
		lapply(dat[dat$sim==i,],
		function(x) rep(x,dat[dat$sim==i,]$C)))
	# ASSIGN LENGTH TO EXPANDED DATASET
	catch$len_true<- rnorm(length(catch$mn_length),
		catch$mn_length,
		catch$mn_length*cv_vbgf)
	catch$len_obs<- round(catch$len_true,0)

	# BIN THE TRUE DATA 
	brks<- seq(0, (max(catch$len_obs)+bin_int),bin_int)
	labs<- c(1:(length(brks)-1))
	catch$len_obs_bin<- cut(catch$len_obs, breaks=brks, labels=labs)
		
	# RANDOMIZE CATCH
	catch$srs<- sample(1:nrow(catch),nrow(catch),replace=FALSE)
	catch<- catch[order(catch$srs,catch$len_obs_bin),]
	bins_obs<-unique(catch$len_obs_bin)
	catch$bin<-0
	
	# RANDOMIZE WITHIN BIN	
	for(x in 1:length(bins_obs))
		{
		catch[catch$len_obs_bin==bins_obs[x],]$bin<-  c(1:nrow(catch[catch$len_obs_bin==bins_obs[x],]))
		}

			
	## [1.1] BINNING ANALYSIS Do this first to so the srs can be fixed to the same smapel size
	## SELECT BINNED SAMPLE AS THE TOTAL SAMPLE SIZE LESS THOSE THAT ARE OVER THE BIN
	samp_bin<- subset(catch, bin<=bin_n)	
	samp_bin<- samp_bin[order(samp_bin$srs),]
	if(nrow(samp_bin)<ss){indx<-c(1:nrow(samp_bin))}else{indx<-c(1:ss)}
	samp_bin<- samp_bin[indx,] 
			
	## [1.2] SIMPLES RANDOM SAMPLE 
	samp_srs<- subset(catch, srs<=nrow(samp_bin))	
		
	# [1] END SAMPLING SETUP	
		
		
	## [3.2] ESTIMATE GROWTH FUNCTION PARAMETERS

	# CALCULATE WEIGHTS
	top<-table(catch$len_obs_bin)/nrow(catch)
	bot<-table(samp_bin$len_obs_bin)/nrow(samp_bin)
	rw<- data.frame(len_obs_bin=levels(catch$len_obs_bin),rw= as.numeric(top/bot))
	samp_bin<- merge(samp_bin, rw, by="len_obs_bin",all.x=TRUE)

	# PROPORTION OF BIN FILLED
	pp<- table(samp_bin$len_obs_bin)
	pp<-pp[pp>0]
	pp<- length(pp[pp==bin_n])/length(pp)
	
	
	
	yy<- data.frame()
	### FIT VBGF TO BINNED DATA		
	fit_srs<- try(nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
		samp_srs,
		start=list(Linf=linf,k=k,t0=t0),
			control=list(maxiter=3000)),silent=TRUE)
	if(class(fit_srs)!="try-error") 
		{y<-as.data.frame(summary(fit_srs)$coefficients[,c(1:2)])
		y$parm<- rownames(y)
		y$type<-"srs"
		y$id<- i
		y$ss<-nrow(samp_srs)
		yy<- rbind(yy,y)
		}
	fit_bin<- try(nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
		samp_bin,
			start=list(Linf=linf,k=k,t0=t0),
			control=list(maxiter=3000)),silent=TRUE)
	if(class(fit_bin)!="try-error") 
		{y<-as.data.frame(summary(fit_bin)$coefficients[,c(1:2)])
		y$parm<- rownames(y)
		y$type<-"bin"
		y$id<- i
		y$ss<-nrow(samp_bin)
		yy<- rbind(yy,y)
		}
	fit_bin_wgt<- try(nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
		samp_bin,weights=rw, 
		start=list(Linf=linf,k=k,t0=t0),
		control=list(maxiter=3000)),silent=TRUE)			
	if(class(fit_bin_wgt)!="try-error") 
		{
		y<-as.data.frame( summary(fit_bin_wgt)$coefficients[,c(1:2)])
		y$parm<- rownames(y)
		y$type<-"rw"
		y$id<- i
		y$ss<-nrow(samp_bin)
		yy<- rbind(yy,y)
		}
		
	# FIT TO MEAN (SRS)
	samp_bin
	mn<- aggregate(len_obs~age,samp_srs,mean)
	mn$var<-  aggregate(len_obs~age,samp_srs,var)[,-1]
	mn$sd<- sqrt(mn$var)
	mn$w<- 1/mn$var
	fit_srs_mn<- try(nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
		mn, 
		start=list(Linf=linf,k=k,t0=t0),
		control=list(maxiter=3000)),silent=TRUE)
	if(class(fit_srs_mn)!="try-error") 
		{y<-as.data.frame( summary(fit_srs_mn)$coefficients[,c(1:2)])
		y$parm<- rownames(y)
		y$type<-"srs_mn"
		y$id<- i
		y$ss<-nrow(samp_bin)
		yy<- rbind(yy,y)
		}

	# fitting to mean
	# j indexes length bin
	# i indexes age
	n_j<- as.matrix(dcast(samp_bin, "n"~len_obs_bin,
		value.var="age",length)[,-1])
	n_ij<- as.matrix(dcast(samp_bin, age~len_obs_bin,
		value.var="age",length)[,-1])
	N_j<-as.matrix(dcast(catch, "n"~len_obs_bin,
		value.var="age",length) [,-1])
	l_bar_ij<- as.matrix(dcast(samp_bin, age~len_obs_bin,
		value.var="len_obs",mean,fill=0)[,-1])
	N_i<- as.matrix(dcast(catch, age~"n",value.var="age",
		length) [,-1])
	N_j<-N_j[rep(1,nrow(n_ij)),]
	n_j<- n_j[rep(1,nrow(n_ij)),]
	N_ij<- N_j*n_ij/n_j
	L_bar_i<- apply(N_ij*l_bar_ij,1,sum)/N_i
	yyy<- data.frame(age=unique(samp_bin$age), L_bar_i = L_bar_i)
	samp_bin<- merge(samp_bin,yyy, by= "age")
	samp_bin$res<- (samp_bin$len_obs-samp_bin$L_bar_i)^2
	res<- as.matrix(dcast(samp_bin,age~len_obs_bin,
		value.var="res",mean,fill=0)[,-1])
	S<- apply(N_ij*res,1,sum)/(N_i-1)
	
	# FIT TO MEAN 	
	fit_bin_mn<- try(nls(L_bar_i~ Linf * (1- exp(-k*(age-t0))),
		yyy,
		start=list(Linf=linf,k=k,t0=t0),
		control=list(maxiter=3000)),silent=TRUE)
	if(class(fit_bin_mn)!="try-error") 
		{
		y<-as.data.frame( summary(fit_bin_mn)$coefficients[,c(1:2)])
		y$parm<- rownames(y)
		y$type<-"srs_mn_wghtd"
		y$id<- i
		y$ss<-nrow(samp_bin)
		yy<- rbind(yy,y)
		}
	# FIT TO MEAN  (SRS) & WEIGHTED BY INVERSE OF VARIANCE 	
	yyy$w<- (1/S)+0.00000001
	
	fit_bin_mn_wgt<- try(nls(L_bar_i~ Linf * (1- exp(-k*(age-t0))),
		yyy, weights=yyy$w,
		start=list(Linf=linf,k=k,t0=t0),
		control=list(maxiter=3000))
		,silent=TRUE)
	if(class(fit_srs_mn_wgt)!="try-error") 
		{
		y<-as.data.frame( summary(fit_srs_mn_wgt)$coefficients[,c(1:2)])
		y$parm<- rownames(y)
		y$type<-"srs_mn_wghtd"
		y$id<- i
		y$ss<-nrow(samp_bin)
		yy<- rbind(yy,y)
		}		
	
	
	
	yy$true<- 1
	yy[yy$parm=="Linf",]$true<- linf
	yy[yy$parm=="k",]$true<- k
	yy[yy$parm=="t0",]$true<- t0	
	yy$bin_n<- bin_n	
	yy$bin_int<- bin_int
	yy$prop_bin_fill<- pp
	
	return(yy)	
	}