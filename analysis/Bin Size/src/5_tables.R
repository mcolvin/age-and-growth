tables<- function(n){

if(n==1)
	{
	
	## SIMULATE LENGTH AT AGE ASSUMING A VBGF
	linf=600
	k=0.25
	t0=0.0
	cv_vbgf=0.1
	# ASSIGN TRUE LENGTH AND OBSERVED LENGTH, LENGTH TO THE NEARST MM
	ss<- 500
	bin_int<- c(10,20)
	bin_n<- 20
	cv_vbgf<-0.1
	samp_size<- 500
	dat<- generate_data(
		nsims=1,
		A=16,
		a_mat=2,
		sigma_rec=0,
		r_median=10000,
		F_median=0,
		sigma_F=0.1)
	dat$mn_length<- linf * (1 - exp(-k * (dat$age-t0)))
	
	# EXPAND CATCH COUNT DATA
	catch<- as.data.frame(lapply(dat, function(x) rep(x,dat$N)))
	# ASSIGN LENGTH TO EXPANDED DATASET
	catch$len_true<- rnorm(length(catch$mn_length),catch$mn_length,catch$mn_length*cv_vbgf)
	catch$len_obs<- round(catch$len_true,0)
	# BIN THE TRUE DATLA 
	brks<- seq(0, (max(catch$len_obs)+bin_int[1]),bin_int[1])
	labs<- paste(brks[-length(brks)],brks[-1],sep=" - ")
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
	# RANDOM SAMPLE ANALYSIS
	catch$id<- c(1:nrow(catch))
	catch$tmp<-1
	samp<- subset(catch,bin<=20) # UP TO 20 IN A BIN
	samp<- samp[c(1:samp_size),] # SAMPLE SIZE
	
	# LENGTH FREQUENCY
	out<- dcast(catch, len_obs_bin~age,value.var="tmp",sum,drop=FALSE,fill=0) 
	out_samp<- dcast(samp, len_obs_bin~age,value.var="tmp",sum,drop=FALSE,fill=0) 
	
	write.csv(out,"./output/steve_lf_all.csv")
	write.csv(out_samp,"./output/steve_lf_n500.csv")
	
	
	
	# 2 cm interval
	# BIN THE TRUE DATLA 
	brks<- seq(0, (max(catch$len_obs)+bin_int[2]),bin_int[2])
	labs<- paste(brks[-length(brks)],brks[-1],sep=" - ")
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
	# RANDOM SAMPLE ANALYSIS
	catch$id<- c(1:nrow(catch))
	catch$tmp<-1
	samp<- subset(catch,bin<=20) # UP TO 20 IN A BIN
	samp<- samp[c(1:samp_size),] # SAMPLE SIZE
	
	# LENGTH FREQUENCY
	out<- dcast(catch, len_obs_bin~age,value.var="tmp",sum,drop=FALSE,fill=0) 
	out_samp<- dcast(samp, len_obs_bin~age,value.var="tmp",sum,drop=FALSE,fill=0) 
	
	write.csv(out,"./output/steve_lf_all2cm.csv")
	write.csv(out_samp,"./output/steve_lf_n5002cm.csv")
	}

}




























