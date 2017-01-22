set.seed(123)
K_bg<- c(0.3,0.4,0.5)
K_cc<- c(0.05,0.1,0.2)
bg_intv<- c(100,125)
cc_intv<- c(400,425)

	linf<-21.5
	K<- K_bg[1]
	t0<-  -0.313
	cv_vbgf<- 0.2
	bin_int<- 2.5
	bin_n<- 1
	ss<- 500
	max_age<-6
	mort<- 0.2
	dat<- generate_data_fixed_A(
		nsims=1,
		A=mort,
		A_max= max_age,
		sigma_rec=0.0,
		r_median=250)
	dat$mn_length<- linf  * (1 - exp(-K * (dat$age-t0))) *10# in mm	
	xx<- expand_and_bin_catch(dat=dat, sim=1,bin_int=bin_int,cv_vbgf=cv_vbgf)
	yy<-makeTransparent("black")
	par(mfrow=c(3,2),mar=c(2,3,0,0),oma=c(3,3,1,1))
	plot(jitter(len_obs)~jitter(age,amount=0.1),xx,las=1,col=yy,xlab="Age",ylab="Length (mm)",ylim=c(0,250))
	abline(h=seq(0,300,by=25),lty=2)
	bg_sub<- subset(xx,len_obs>bg_intv[1] &len_obs<bg_intv[2])
	bg_sub$i<-1
	ages<- subset(xx,len_obs>bg_intv[1] &len_obs<bg_intv[2])$age
	n_ages<- data.frame(relFreq=round(prop.table(table(ages)),2),nxt="\n")
	text(5,50,paste("Number of unique \n ages in interval: ", length(unique(ages)),collapse=""))

	
	
	# Channel catfish
	linf<-84.4
	K<- K_cc[1]
	t0<-  0.669
	cv_vbgf<- 0.2
	bin_int<- 2.5
	bin_n<- 1
	ss<- 500
	max_age<-31
	mort<- 0.2
	dat<- generate_data_fixed_A(
		nsims=1,
		A=mort,
		A_max= max_age,
		sigma_rec=0.0,
		r_median=250)
	dat$mn_length<- linf  * (1 - exp(-K * (dat$age-t0))) *10# in mm	
	xx<- expand_and_bin_catch(dat=dat, sim=1,bin_int=bin_int,cv_vbgf=cv_vbgf)
	yy<-makeTransparent("black")
	plot(jitter(len_obs)~jitter(age,amount=0.1),xx,las=1,col=yy,xlab="Age",ylab="Length (mm)",ylim=c(0,1000))
	abline(h=cc_intv,lty=2)	
	cc_sub<- subset(xx,len_obs>cc_intv[1] &len_obs<cc_intv[2])
	cc_sub$i<-1	
		ages<- subset(xx,len_obs>cc_intv[1] &len_obs<cc_intv[2])$age
	n_ages<- data.frame(relFreq=round(prop.table(table(ages)),2),nxt="\n")
	text(25,75,paste("Number of unique \n ages in interval: ", length(unique(ages)),collapse=""))
	
	
	linf<-21.5
	K<- K_bg[2]
	t0<-  -0.313
	cv_vbgf<- 0.2
	bin_int<- 2.5
	bin_n<- 1
	ss<- 500
	max_age<-6
	mort<- 0.2
	dat<- generate_data_fixed_A(
		nsims=1,
		A=mort,
		A_max= max_age,
		sigma_rec=0.0,
		r_median=250)
	dat$mn_length<- linf  * (1 - exp(-K * (dat$age-t0))) *10# in mm	
	xx<- expand_and_bin_catch(dat=dat, sim=1,bin_int=bin_int,cv_vbgf=cv_vbgf)
	yy<-makeTransparent("black")
	plot(jitter(len_obs)~jitter(age,amount=0.1),xx,las=1,col=yy,xlab="Age",ylab="Length (mm)",ylim=c(0,250))
	abline(h=bg_intv,lty=2)
	bg_app<- subset(xx,len_obs>bg_intv[1] &len_obs<bg_intv[2])
	bg_app$i<-2
	bg_sub<- rbind(bg_sub,bg_app)
	ages<- subset(xx,len_obs>bg_intv[1] &len_obs<bg_intv[2])$age
	n_ages<- data.frame(relFreq=round(prop.table(table(ages)),2),nxt="\n")
	text(5,50,paste("Number of unique \n ages in interval: ", length(unique(ages)),collapse=""))

	
	# Channel catfish
	linf<-84.4
	K<- K_cc[2]
	t0<-  0.669
	cv_vbgf<- 0.2
	bin_int<- 2.5
	bin_n<- 1
	ss<- 500
	max_age<-31
	mort<- 0.2
	dat<- generate_data_fixed_A(
		nsims=1,
		A=mort,
		A_max= max_age,
		sigma_rec=0.0,
		r_median=250)
	dat$mn_length<- linf  * (1 - exp(-K * (dat$age-t0))) *10# in mm	
	xx<- expand_and_bin_catch(dat=dat, sim=1,bin_int=bin_int,cv_vbgf=cv_vbgf)
	yy<-makeTransparent("black")
	plot(jitter(len_obs)~jitter(age,amount=0.1),xx,las=1,col=yy,xlab="Age",ylab="Length (mm)",ylim=c(0,1000))
	abline(h=cc_intv,lty=2)	
	cc_app<- subset(xx,len_obs>cc_intv[1] &len_obs<cc_intv[2])
	cc_app$i<-2
	cc_sub<- rbind(cc_sub,cc_app)	
	ages<- subset(xx,len_obs>cc_intv[1] &len_obs<cc_intv[2])$age
	n_ages<- data.frame(relFreq=round(prop.table(table(ages)),2),nxt="\n")
	text(25,75,paste("Number of unique \n  ages in interval: ", length(unique(ages)),collapse=""))
	
	linf<-21.5
	K<- K_bg[3]
	t0<-  -0.313
	cv_vbgf<- 0.2
	bin_int<- 2.5
	bin_n<- 1
	ss<- 500
	max_age<-6
	mort<- 0.2
	dat<- generate_data_fixed_A(
		nsims=1,
		A=mort,
		A_max= max_age,
		sigma_rec=0.0,
		r_median=250)
	dat$mn_length<- linf  * (1 - exp(-K * (dat$age-t0))) *10# in mm	
	xx<- expand_and_bin_catch(dat=dat, sim=1,bin_int=bin_int,cv_vbgf=cv_vbgf)
	yy<-makeTransparent("black")
	plot(jitter(len_obs)~jitter(age,amount=0.1),xx,las=1,col=yy,xlab="Age",ylab="Length (mm)",ylim=c(0,250))
	abline(h=bg_intv,lty=2)
	bg_app<- subset(xx,len_obs>bg_intv[1] &len_obs<bg_intv[2])
	bg_app$i<-3
	bg_sub<- rbind(bg_sub,bg_app)
	ages<- subset(xx,len_obs>bg_intv[1] &len_obs<bg_intv[2])$age
	n_ages<- data.frame(relFreq=round(prop.table(table(ages)),2),nxt="\n")
	text(5,50,paste("Number of unique \n ages in interval: ", length(unique(ages)),collapse=""))
	
	# Channel catfish
	linf<-84.4
	K<- K_cc[3]
	t0<-  0.669
	cv_vbgf<- 0.2
	bin_int<- 2.5
	bin_n<- 1
	ss<- 500
	max_age<-31
	mort<- 0.2
	dat<- generate_data_fixed_A(
		nsims=1,
		A=mort,
		A_max= max_age,
		sigma_rec=0.0,
		r_median=250)
	dat$mn_length<- linf  * (1 - exp(-K * (dat$age-t0))) *10# in mm	
	xx<- expand_and_bin_catch(dat=dat, sim=1,bin_int=bin_int,cv_vbgf=cv_vbgf)
	yy<-makeTransparent("black")
	plot(jitter(len_obs)~jitter(age,amount=0.1),xx,las=1,col=yy,xlab="Age",ylab="Length (mm)",ylim=c(0,1000))
		abline(h=cc_intv,lty=2)
	cc_app<- subset(xx,len_obs>cc_intv[1] &len_obs<cc_intv[2])
	cc_app$i<-3
	cc_sub<- rbind(cc_sub,cc_app)	
		ages<- subset(xx,len_obs>cc_intv[1] &len_obs<cc_intv[2])$age
	n_ages<- data.frame(relFreq=round(prop.table(table(ages)),2),nxt="\n")
	text(25,75,paste("Number of unique ages in interval: ", length(unique(ages)),collapse=""))
	
	tbl<-dcast(bg_sub, i~age,value.var='i', fun=length)
	tbl<-dcast(cc_sub, i~age,value.var='i', fun=length)