install.packages("FSA")

source("C:/Users/mcolvin/Documents/projects/Age and Growth/Analysis/global/global_functions.R")


run_mc<- function(w)
	{
	h<- ddd[w]
	linf<- combos$linf[h]
	K<- combos$K[h]
	t0<- combos$t0[h]
	cv_vbgf<- combos$cv_vbgf[h]
	bin_int<- combos$bin_int[h]
	bin_n<- combos$bin_n[h]
	ss<- combos$ss[h]
	max_age<- combos$a_max[h]
	mort<- combos$Annual_mortality[h]
	dat<- generate_data_fixed_A(
		nsims=1000,
		A=mort,
		A_max= max_age,
		sigma_rec=0.0,
		r_median=10000)
	dat$mn_length<- linf  * (1 - exp(-K * (dat$age-t0))) *10# in mm	
	out_vbgf<- data.frame()
	out<- data.frame()
	len_dis<- data.frame()
	age_dist<-data.frame()
	tm<-c()
	for(i in 1:20)
		{
		ptm <- proc.time()
		xx<- expand_and_bin_catch(dat=dat, sim=i,bin_int=bin_int,cv_vbgf=cv_vbgf)
		xx$age_f<- as.factor(xx$age)
		
		# LENGTH SAMPLE
		samp<- subset(xx,srs<= ss) 
		# ALK SAMPLE
		samp_bin<- subset(samp, bin<= bin_n)
	
	
		# LENGTH AND AGE [MEAN, MAX] ########################################################################
		out_app<- data.frame(combo=h, rep=i,
			### POPULATION MEAN AGE
			pop_age_mean_true=mean(xx$age),
			pop_len_mean_true=mean(xx$len_obs),	
			pop_age_max_true=max(xx$age),
			## SRS SAMPLE 
			srs_mn_age=mean(samp$age),
			srs_mn_len=mean(samp$len_obs),	
			srs_mx_age=max(samp$age),			
			## ALK SAMPLE
			alk_mn_age=mean(samp_bin$age),
			alk_mn_len=mean(samp_bin$len_obs),
			alk_mx_age=max(samp_bin$age))
		
		# END LENGTH AND AGE [MEAN, MAX] ####################################################################
	
	
	
		# LENGTH AND AGE DISTRIBUTION #######################################################################
		
		## LENGTH DISTRIBUTION [POP, SRS, ALK]
		len_dist_app<- dcast(xx, len_obs_bin~"n_pop", value.var="len_obs_bin",length,drop=FALSE)
		len_dist_app$n_srs<- dcast(samp, len_obs_bin~"n_srs", value.var="len_obs_bin",length,drop=FALSE)[,-1]
		len_dist_app$n_alk<- dcast(samp_bin, len_obs_bin~"n_alk", value.var="len_obs_bin",length,drop=FALSE)[,-1]
		len_dist_app$p_pop<- len_dist_app$n_pop/sum(len_dist_app$n_pop)
		len_dist_app$p_srs<- len_dist_app$n_srs/sum(len_dist_app$n_srs)
		len_dist_app$p_alk<- len_dist_app$n_alk/sum(len_dist_app$n_alk)
		len_dist_app$combo<-h
		len_dist_app$rep<-i
		
		
		## AGE DISTRIBUTION [POP, SRS, ALK]
		age_dist_app<- dcast(xx, age_f~"n_pop", value.var="len_obs_bin",length,drop=FALSE)
		age_dist_app$n_srs<- dcast(samp, age_f~"n_srs", value.var="len_obs_bin",length,drop=FALSE)[,-1]
		age_dist_app$n_alk<- dcast(samp_bin, age_f~"n_alk", value.var="len_obs_bin",length,drop=FALSE)[,-1]
		age_dist_app$p_pop<- age_dist_app$n_pop/sum(age_dist_app$n_pop)
		age_dist_app$p_srs<- age_dist_app$n_srs/sum(age_dist_app$n_srs)
		age_dist_app$p_alk<- age_dist_app$n_alk/sum(age_dist_app$n_alk)
		age_dist_app$combo<-h
		age_dist_app$rep<-i
		
		
		### METRICS
		#len_dist_sim<- sum(apply(cbind(len_dist_true$p,len_dist_obs$p),1,min))# SIMILARITY
		#len_mean_prop<- (len_mean_obs-len_mean_true)/len_mean_true
		out<- rbind(out,out_app)
		len_dis<- rbind(len_dis,len_dist_app)
		age_dist<- rbind(age_dist,age_dist_app)



		# VBGF ###################################################
		
		# ESTIMATE VALUES, DISTRIBUTIONS AND PARAMETERS
		samp$len_obs<- round(samp$len_true,0)/10 # convert back to cm to jive with Linf and k	
		samp_bin$len_obs<- round(samp_bin$len_true,0)/10 # convert back to cm to jive with Linf and k	

		## FIT VBGF TO DATA WITH NO CORRECTION	
		y<- vbgf_fit(samp=samp_bin, linf=linf, k=K, t0=t0)	
		out_vbgf<- rbind(out_vbgf,
			data.frame(i=i, combo=h, type="No correction",parm=rownames(y),est=y[,1],se=y[,2],true=c(linf,K,t0)))
		
		## FIT VBGF TO MEAN LENGTH AT AGE
		# j indexes length bin, i indexes age
		n_j<- as.matrix(dcast(samp_bin, "n"~len_obs_bin,
			value.var="age",length)[,-1])
		n_ij<- as.matrix(dcast(samp_bin, age~len_obs_bin,
			value.var="age",length)[,-1])
		N_j<-as.matrix(dcast(samp, "n"~len_obs_bin,
			value.var="age",length) [,-1],drop=FALSE)
		l_bar_ij<- as.matrix(dcast(samp_bin, age~len_obs_bin,
			value.var="len_obs",mean,fill=0)[,-1])
		N_j<-N_j[rep(1,nrow(n_ij)),]
		n_j<- n_j[rep(1,nrow(n_ij)),]
		N_ij<- N_j*n_ij/n_j
		N_i<- apply(N_ij,1,sum)
		L_bar_i<- apply(N_ij*l_bar_ij,1,sum)/N_i
		yyy<- data.frame(age=sort(unique(samp_bin$age)), len_obs = L_bar_i)
		y<- vbgf_fit(samp=yyy, linf=linf, k=K, t0=t0)	
		out_vbgf<- rbind(out_vbgf,
			data.frame(i=i, combo=h, type="mla",parm=rownames(y),est=y[,1],se=y[,2],true=c(linf,K,t0)))
		
		# WEIGHTING
		top<-table(samp$len_obs_bin)/nrow(samp)
		bot<-table(samp_bin$len_obs_bin)/nrow(samp_bin)
		rw<- data.frame(len_obs_bin=levels(samp$len_obs_bin),rw= as.numeric(top/bot))
		samp_bin<- merge(samp_bin, rw, by="len_obs_bin",all.x=TRUE)
		y<- vbgf_fit_rw(samp=samp_bin, linf=linf, k=K, t0=t0,w=samp_bin$rw)
		out_vbgf<- rbind(out_vbgf,
			data.frame(i=i, combo=h,type="rw",parm=rownames(y),est=y[,1],se=y[,2],true=c(linf,K,t0)))
		tm<- c(tm,(proc.time()-ptm)[3])
		print(tm[i])
		} #end i
		
		fn<- paste("./output/out_vbgf",h,".csv",sep="_")
		write.csv(out_vbgf,fn)
		fn<- paste("./output/out",h,".csv",sep="_")
		write.csv(out,fn)
		fn<- paste("./output/len_dis",h,".csv",sep="_")		
		write.csv(len_dis,fn)
		fn<- paste("./output/age_dist",h,".csv",sep="_")		
		write.csv(age_dist,fn)

}

