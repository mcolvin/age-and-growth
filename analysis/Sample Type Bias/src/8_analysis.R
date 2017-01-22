# SSF: Short, small, fast growing
	nsims<- 1000  
	dat<- generate_data(
		nsims=nsims,
		A=8,
		a_mat=2,
		sigma_rec=0,
		r_median=10000,
		F_median=0.8,
		sigma_F=0)
	
	linf<-vbgf_parms$linf[1]
	k	<-vbgf_parms$k[1]	
	t0	<-vbgf_parms$t0
	dat$mn_length<- linf  * (1 - exp(-k * (dat$age-t0)))	
	out<- data.frame()
	for(i in 1:max(dat$sim) )
		{
		tmp<- analysis(i=i,cv_vbgf=0.1,
			bin_int=10,bin_n=25,ss=1000,
			linf=vbgf_parms$linf[1],k=vbgf_parms$k[1],t0=vbgf_parms$t0[1])
		out<- rbind(out,tmp)
		}	
	out$lh<- "ssf"
	all_types<- out
	write.csv(all_types,"./output/all_types.csv")	
	
# LLS: Long lives, large, slow growing
	dat<- generate_data(
		nsims=nsims,
		A=40,
		a_mat=10,
		sigma_rec=0,
		r_median=10000,
		F_median=0.3,
		sigma_F=0)
	linf<-vbgf_parms$linf[2]
	k	<-vbgf_parms$k[2]		
	t0	<-vbgf_parms$t0[2]
	dat$mn_length<- linf  * (1 - exp(-k * (dat$age-t0)))	
	out<- data.frame()
	for(i in 1:max(dat$sim))
		{
		tmp<- analysis(i=i,cv_vbgf=0.1,
			bin_int=10,bin_n=25,ss=1000,
			linf=vbgf_parms$linf[2],k=vbgf_parms$k[2],t0=vbgf_parms$t0[2])
		out<- rbind(out,tmp)
		}
	out$lh<- "lls"
	all_types<-rbind(all_types,out)
	write.csv(all_types,"./output/all_types.csv")

	# LLS: Long lives, large, slow growing
	dat<- generate_data(
		nsims=nsims,
		A=40,
		a_mat=10,
		sigma_rec=0,
		r_median=10000,
		F_median=0.1,
		sigma_F=0)
	linf<-vbgf_parms$linf[2]
	k	<-vbgf_parms$k[2]		
	t0	<-vbgf_parms$t0[2]
	dat$mn_length<- linf  * (1 - exp(-k * (dat$age-t0)))	
	out<- data.frame()
	for(i in 1:max(dat$sim))
		{
		tmp<- analysis(i=i,cv_vbgf=0.1,
			bin_int=25,bin_n=10,ss=1000,
			linf=vbgf_parms$linf[2],k=vbgf_parms$k[2],t0=vbgf_parms$t0[2])
		out<- rbind(out,tmp)
		}
	out$lh<- "lls"
	all_types<-rbind(all_types,out)
	write.csv(all_types,"./output/all_types.csv")
	
	
		# LLS: Long lives, large, slow growing
	dat<- generate_data(
		nsims=nsims,
		A=40,
		a_mat=10,
		sigma_rec=0,
		r_median=10000,
		F_median=0.1,
		sigma_F=0)
	linf<-vbgf_parms$linf[2]
	k	<-vbgf_parms$k[2]		
	t0	<-vbgf_parms$t0[2]
	dat$mn_length<- linf  * (1 - exp(-k * (dat$age-t0)))	
	out<- data.frame()
	for(i in 1:max(dat$sim))
		{
		tmp<- analysis(i=i,cv_vbgf=0.1,
			bin_int=10,bin_n=10,ss=1000,
			linf=vbgf_parms$linf[2],k=vbgf_parms$k[2],t0=vbgf_parms$t0[2])
		out<- rbind(out,tmp)
		}
	out$lh<- "lls"
	all_types<-rbind(all_types,out)
	write.csv(all_types,"./output/all_types.csv")
	
			# LLS: Long lives, large, slow growing
	dat<- generate_data(
		nsims=nsims,
		A=40,
		a_mat=10,
		sigma_rec=0,
		r_median=10000,
		F_median=0.1,
		sigma_F=0)
	linf<-vbgf_parms$linf[2]
	k	<-vbgf_parms$k[2]		
	t0	<-vbgf_parms$t0[2]
	dat$mn_length<- linf  * (1 - exp(-k * (dat$age-t0)))	
	out<- data.frame()
	for(i in 1:max(dat$sim))
		{
		tmp<- analysis(i=i,cv_vbgf=0.1,
			bin_int=25,bin_n=25,ss=1000,
			linf=vbgf_parms$linf[2],k=vbgf_parms$k[2],t0=vbgf_parms$t0[2])
		out<- rbind(out,tmp)
		}
	out$lh<- "lls"
	all_types<-rbind(all_types,out)
	write.csv(all_types,"./output/all_types.csv")
# HOW DOES THE AGE LENGTH KEY COMPARE TO REWIEGHTING OF SIZE STRUCTURE?




	
	# WHY DOES REWEIGHTING PERFORM SO POORLY FOR 
	# BINS THAT ARE NOT FULL?
	dat<- generate_data(nsims=1,
		A=40,
		a_mat=10,
		sigma_rec=0,
		r_median=10000,
		F_median=0.1,
		sigma_F=0)
	linf<-vbgf_parms$linf[2]
	k	<-vbgf_parms$k[2]		
	t0	<-vbgf_parms$t0[2]
	cv_vbgf=0.1,
	dat$mn_length<- linf  * (1 - exp(-k * (dat$age-t0)))	
	# EXPAND CATCH COUNT DATA	
	catch<- as.data.frame(
		lapply(dat[dat$sim==1,],
		function(x) rep(x,dat[dat$sim==1,]$C)))
	# ASSIGN LENGTH TO EXPANDED DATASET
	catch$len_true<- rnorm(length(catch$mn_length),
		catch$mn_length,
		catch$mn_length*0.1)
	catch$len_obs<- round(catch$len_true,0)

	bin_int=25
	bin_n=25
	ss=1000
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
	bin_n=1000
	samp_bin<- subset(catch, bin<=bin_n)	
	samp_bin<- samp_bin[order(samp_bin$srs),]
	if(nrow(samp_bin)<ss){indx<-c(1:nrow(samp_bin))}else{indx<-c(1:ss)}
	samp_bin<- samp_bin[indx,] 
	# CALCULATE WEIGHTS
	top<-table(catch$len_obs_bin)/nrow(catch)
	bot<-table(samp_bin$len_obs_bin)/nrow(samp_bin)
	rw<- data.frame(len_obs_bin=levels(catch$len_obs_bin),rw= as.numeric(top/bot))
	samp_bin<- merge(samp_bin, rw, by="len_obs_bin",all.x=TRUE)

	
	presPlot()
	barplot(t(cbind(top[-c(1:25)],bot[-c(1:25)])),beside=TRUE)
	legend("topright","Random sample", "Age Length Key"), 
	
	
	
	