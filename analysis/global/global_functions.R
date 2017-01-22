# THIS FILE CONTAINS FUNCTIONS THAT ARE USED IN THE MAJORITY OF
# ANALYSES IN THIS FOLDER.  IT SERVES AS THE CENTRAL FUNCTION REPOSITORY
# AND THEREFORE THERE IT IS ONLY NEEDED TO UPDATE FUNCTIONS HERE 
# FOR USE IN ALL ANALYSES

expand_and_bin_catch<- function(dat, sim=1,bin_int,cv_vbgf)
	{# EXPAND CATCH COUNT DATA
	catch<- as.data.frame(
		lapply(dat[dat$sim==sim,],
		function(x) rep(x,dat[dat$sim==sim,]$N)))
	# ASSIGN LENGTH TO EXPANDED DATASET
	catch$len_true<- rnorm(length(catch$mn_length),catch$mn_length,
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
	# [1] END SAMPLING SETUP
	return(catch)
	}


vbgf_fit<- function(samp, linf, k, t0)
	{
	fit<- try(
			nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
			samp,
			start=list(
				Linf=linf, 
				k=k, 
				t0=t0),
			control=list(maxiter=3000)),
			silent=T)
	
		if(class(fit)=="try-error") 
			{
			xxxx<- sort(c(1.2*t0,0.7*t0))
			# BRUTE FORCE FOR NLS STARTS
			starts<- data.frame(Linf=runif(2500,0.7*linf,1.2*linf),
				k=runif(2500,0.7*k,1.2*k),
				t0=runif(2500,xxxx[1],xxxx[2]))
			mod1 <- nls2(len_obs~ Linf * (1- exp(-k*(age-t0))),
				samp,
				start = starts,
				algorithm = "brute-force")
			fit<- try(nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
				samp,
				start =  coef(mod1),
				control=list(maxiter=3000)),silent=T)
				}				
		if(class(fit)=="try-error") 
			{
			y<- data.frame(est=rep(-99,3),se=rep(-99,3))
			rownames(y)<-c("Linf","k","t0")
			}
		if(class(fit)!="try-error") 
			{
			y<-summary(fit)$coefficients[,c(1:2)]
			}
	return(y)
} # end function

vbgf_fit_rw<- function(samp, linf, k, t0,w)
	{
	fit<- try(
			nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
			samp,weights=w,
			start=list(
				Linf=linf, 
				k=k, 
				t0=t0),
			control=list(maxiter=3000)),
			silent=T)
	
		if(class(fit)=="try-error") 
			{
			xxxx<- sort(c(1.2*t0,0.7*t0))
			# BRUTE FORCE FOR NLS STARTS
			starts<- data.frame(Linf=runif(2500,0.7*linf,1.2*linf),
				k=runif(2500,0.7*k,1.2*k),
				t0=runif(2500,xxxx[1],xxxx[2]))
			mod1 <- nls2(len_obs~ Linf * (1- exp(-k*(age-t0))),
				samp,weights=w,
				start = starts,
				algorithm = "brute-force")
			fit<- try(nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
				samp,weights=w,
				start =  coef(mod1),
				control=list(maxiter=3000)),silent=T)
				}				
		if(class(fit)=="try-error") 
			{
			y<- data.frame(est=rep(-99,3),se=rep(-99,3))
			rownames(y)<-c("Linf","k","t0")
			}
		if(class(fit)!="try-error") 
			{
			y<-summary(fit)$coefficients[,c(1:2)]
			}
	return(y)
} # end function

generate_data<- function(# generate age and catch data
	nsims=1000,
	A=8, # MAXIMUM AGE
	a_mat=2,
	# RECRUITMENT INDEPENDENT OF POPULATION
	sigma_rec=0.3,
	r_median=1000000,
	F_median=1.4,# BASED ON LIFE HISTORY TYPE
	sigma_F=0.1) # 0.1 or 0.3
	{
	out<- data.frame()
	nyears<- A 
	M=exp(1.42+-0.982*log(A)) # Hoenig 1983 for all critters
	out<- data.frame()
	for(k in 1:nsims)
		{
		# TEMPORAL VARIABILITY IN RECRUITMENT
		R <- r_median*exp(rnorm(nyears,0, sigma_rec))
		# TEMPORAL VARIABILITY IN FISHING MORTALITY
		F_t<- F_median*exp(rnorm(nyears,0,sigma_F))
		# POPULATION DYNAMICS
		N<-Z<-F<-C<- matrix(0,A,nyears)
		N[1,]<- R
		F[a_mat,]<- 0.5*F_t
		for(i in (a_mat+1):A){F[i,]<- F_t*1}
		Z<- F+M
		for(i in 2:nyears)
			{
			N[-1,i]<- (N[,i-1]*exp(-Z[,i]))[-A]
			}
		C<- round(N*(1-exp(-Z))*(F/Z),0) # CATCH
		N<- round(N,0)
		out_app<- data.frame(age=c(1:A), N=N[,nyears],C=C[,nyears], sim=k)
		out<- rbind(out,out_app)
		}		
		return(out)
		}

generate_data_fixed_A<- function(
	# generate age and catch data
	# given a fixed annual mortality
	# removed catch 
	nsims=1,
	A=0.2,
	A_max= 8, # MAXIMUM AGE
	# RECRUITMENT INDEPENDENT OF POPULATION
	sigma_rec=0.0,
	r_median=10000)
	{
	out<- data.frame()
	nyears<- 100
	Z=-1*log(1-A)
	out<- data.frame()
	for(k in 1:nsims)
		{
		# TEMPORAL VARIABILITY IN RECRUITMENT
		R <- r_median*exp(rnorm(nyears,0, sigma_rec))
		# TEMPORAL VARIABILITY IN FISHING MORTALITY
		#F_t<- F_median*exp(rnorm(nyears,0,sigma_F))
		# POPULATION DYNAMICS
		N<-matrix(0,A_max,nyears)
		N[1,]<- R
		for(i in 2:nyears)
			{
			N[-1,i]<- (N[,i-1]*exp(-Z))[-nrow(N)]
			}
		N<- round(N[,nyears],0)
		out_app<- data.frame(age=c(1:A_max), N=N, sim=k)
		out<- rbind(out,out_app)
		}		
		return(out)
		}		



srs_data<- function(# calculate true and sample values
	dat=xx,
	linf=300,
	k=0.37,
	t0=0,
	cv_vbgf=0.1,
	ss = c(100,200,500,1000,1500)) # vector of sample sizes
	{
	# TRUE DATA	
	## MEAN LENGTH: JUST BE THE WEIGHTED MEAN OF AGE SPECIFIC MEAN LENGTHS...
	age<- c(1:max(dat$age))
	mn_length<- linf * (1 - exp(-k * (age-t0)))
	rr<- dcast(dat,sim~age,value.var="N",sum)
	mn_len<- data.frame(type="true",sim=1:nrow(rr), ss=0, mean_length=apply(mn_length*t(rr),2,sum))		
	# TRUE AGE STRUCTURE 
	rr<- rr[,-1]/apply(rr[,-1],1,sum)	
	rrr<- data.frame(type="true",sim=j, ss=0, age=c(1:A),val=as.vector(t(rr)))
	# TRUE MEAN LENGTH
	out<- list(mn_len= mn_length, age_structure= rrr)
	# SRS

	for(i in 1:length(ss))
		{
		for(j in 1:max(dat$sim))
			{
			p<- dat[dat$sim==j,]$N/sum(dat[dat$sim==j,]$N)
			ages<-sample(x=dat[dat$sim==j,]$age,
				ss[i],
				replace=TRUE,
				prob=p)
			len<- mn_length[ages]
			len<- rnorm(ss[i],len,len*cv_vbgf)
			ages<- factor(ages, levels=c(1:A))
			
			# MEAN LENGTH
			mn_len_app<- data.frame(type="srs",sim=j, ss=ss[i], mean_length=mean(len))			
			out$mn_len<- rbind(out$mn_len,mn_len_app)			
			# AGE STRUCTURE
			rrr_app<- data.frame(type="srs",sim=j, ss=ss[i], age=c(1:A),val=as.vector(prop.table(table(ages))))
			out$age_structure<- rbind(out$age_structure,rrr_app)
			}
		}
	}



		
summarize<- function(dat,true,Linf, k, t0,maxAge)
	{
	alldat<- rbind(dat,data.frame(cbind(rep=0,type="true",true)))
	res<- list()
	# SIMULATED DATA
	## MEAN AGE
	out<- ddply(alldat,"rep",summarise,
		meanAge= mean(age),
		maxAge=max(age))
	res$age<- out
		
	## AGE STRUCTURE
	y<- dcast(alldat,rep~age,value.var="age",drop=F, fill=0,length)
	y_n<- dcast(alldat,rep~"n",value.var="age",drop=F, fill=0,length)
	out<- data.frame(cbind(y[,1],y[,-1]/y_n[,-1]))
	names(out)<-c("rep",paste("prop_age",c(1:maxAge),sep="_"))
	res$age_structure<- out
	
	## GROWTH CURVE 
	out<- data.frame()
	for(rep in 1:nsim)#, function(x)
		{
		fit<- try(nls(length~Linf * (1- exp(-k*(age-t0))), 
			samples, subset=rep==rep, 
			start=list(
				Linf=Linf, 
				k=k, 
				t0=t0),
			control=list(maxiter=2000)),silent=T)
		if(class(fit)=="try-error") 
			{
			y<- data.frame(est=rep(-99,3),se=rep(-99,3))
			rownames(y)<-c("Linf","k","t0")
			}
		if(class(fit)!="try-error") 
			{
			y<-summary(fit)$coefficients[,c(1:2)]
			}
		app<- data.frame(rep=rep, parameter = rownames(y), est=y[,1], se=y[,2],cv=y[,2]/y[,1])
		out<- rbind(out, app)
		}
	app<- data.frame(rep=0, parameter =c("Linf","k","t0"),est=c(Linf,k,t0),se=rep(0,3),cv=rep(0,3))
	res$growthcurve<- rbind(out,app)	
	return(res)
	}