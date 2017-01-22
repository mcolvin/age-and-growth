tables<- function(n){

if(n==1)
	{# combos tables only run if needed as it is saved in 
	# output.accdb
	## THE CODE BELOW ARE THE INPUTS FOR THE VARIOUS 
	## COMBOS EVALUATED AND SAVED IN THE COMBOS TABLE IN AGE_GROWTH_DATA.ACCDB
	ss<-  c(25,50,seq(100,1000,100)) # SAME AS MIRANDA 
	bin_int<- c(10,25)# IN MM	# interval<- 0.5*round(linf/30/0.5)# round to nearest 5 mm
	bin_n<- c(10,20,30,40,50)
	Annual_mortality<- c(20, 40, 60)/100 # %
	cv_vbgf<-c(0.1	,0.2)
	A<-seq(4,30,2) # MAXIMUM AGE
	a_mat<-1 # AGE AT MATURITY
	sigma_re<-0.3 # RECRUITMENT CV
	r_median<-10000# MEDIAN RECRUITMENT
	F_median<-0 # MEDIAN FISHING MORTALITY
	sigma_F<-0.1 # FISHING MORTALITY CV
	
	combos<- 	expand.grid(ss=ss,
		bin_int=bin_int,
		bin_n=bin_n,
		Annual_mortality=Annual_mortality,
		cv_vbgf=0.1)
	combos$linf<- fish_dat$linf[1]
	combos$K<- fish_dat$K[1]
	combos$t0<- fish_dat$t0[1]
	combos$a_max=fish_dat$tmax[1]
	combos$spp<- fish_dat$Species[1]
	app<- combos

	for(i in 2:nrow(fish_dat))
		{
		app$linf<- fish_dat$linf[i]
		app$K<- fish_dat$K[i]
		app$t0<- fish_dat$t0[i]
		app$a_max=fish_dat$tmax[i]
		combos$spp<- fish_dat$Species[i]
		combos<- rbind(combos, app)
		}
	combos$combo<- 1:nrow(combos)
	return(combos)
	}
if(n==2)
	{
	# THIS FUNCTION COMPILES GROWTH DATA 
	# SIMULATIONS THAT ARE DONE BUT ARE NOT IN THE 
	# DATABASE YET
	fn<- dir("./output")
	fn<- fn[!(fn%in%c("combos.csv","output.accdb",
		"output.laccdb","combos.csv","struc_all.csv","vbgf_all.csv","struc_app.csv","vbgf_app.csv" ))]
	fn1<- matrix(unlist(strsplit(fn,"_")),ncol=4,byrow=TRUE)
	fn<- data.frame(fp=paste("./output/",fn,sep=""),
		type=fn1[,2],
		combo=fn1[,3])
	vbgf<- subset(fn,type=="vbgf" & !(combo %in% combos_in_db))
	vbgf_dat<- data.frame()
	for(i in 1:nrow(vbgf))
		{
		app<- read.csv(as.character(vbgf$fp[i]))
		vbgf_dat<- rbind(vbgf_dat,app)	
		}
	return(vbgf_dat)
	}
if(n==3)
	{
	# THIS FUNCTION COMPILES AGE STRUCTURE DATA 
	# SIMULATIONS THAT ARE DONE BUT ARE NOT IN THE 
	# DATABASE YET
	fn<- dir("./output")
	fn<- fn[!(fn%in%c("combos.csv","output.accdb",
		"output.laccdb","combos.csv","struc_all.csv","vbgf_all.csv","struc_app.csv","vbgf_app.csv" ))]
	fn1<- matrix(unlist(strsplit(fn,"_")),ncol=4,byrow=TRUE)
	fn<- data.frame(fp=paste("./output/",fn,sep=""),
		type=fn1[,2],
		combo=fn1[,3])
	struc<- subset(fn,type=="bin"& !(combo %in% combos_in_db))
	struc_dat<- data.frame()
	for(i in 1:nrow(struc))
		{
		app<- read.csv(as.character(struc$fp[i]))
		struc_dat<- rbind(struc_dat,app)	
		}
	return(struc_dat)
	}
if(n ==4)
	{
	vbgf$bias<- vbgf$est-vbgf$true
	out<- dcast(vbgf,ss+bin_int+Annual_mortality+
		spp+type+parm~"mn_bias",value.var="bias",mean)
	
	out<- ddply(vbgf, .(ss,bin_int,bin_n,
		Annual_mortality,
		spp,type,parm),summarise,
		mn_bias=mean(bias))
		
	xyplot(mn_bias~ss|bin_int+parm, out,
		group=bin_n,type='l',
		subset=(spp=="Norther Pike"& type=="mla" &
		Annual_mortality==0.2))
	}
if(n==5)
	{
	# THIS TABLE DETERMINES THE MINIMUM NUMBER OF SAMPLES NEEDED TO BE WITH 10% OF THE TRUE VALUE
	
	tbldat<-sqlFetch(com3, "structure_out")
	tbldat$ss_age<- tbldat$ss
	tbldat$ss_len<- tbldat$ss
	tbldat$ss_age_com<- tbldat$ss 
	tbldat$ss_len_com<- tbldat$ss	
	
	tbldat[tbldat$AvgOfmean_age<=0.8,]$ss_age<- NA 
	tbldat[tbldat$AvgOfmean_len<=0.8,]$ss_len<- NA
	
	tbldat[tbldat$AvgOfage_com<=0.8,]$ss_age_com<- NA 
	tbldat[tbldat$AvgOflen_com<=0.8,]$ss_len_com<- NA	
	
	out<-ddply(tbldat,
		.(spp, bin_int, bin_n,Annual_mortality),
		summarise,
		n=length(ss),
		ss_mn_age=min(na.omit(ss_age)),
		ss_mn_len=min(na.omit(ss_len)),
		ss_len_com=min(na.omit(ss_len_com)),
		ss_age_com=min(na.omit(ss_age_com)))		
	out[out==Inf]<- NA	
	
	write.csv(out,"./tables/samplesize.csv")
	
	xyplot(ss_age_com~ss_len_com,out)
	xyplot(ss_age_com~ss_len_com|spp+as.factor(bin_int), out, 
		group=bin_n, subset=(Annual_mortality==0.2))
	

	xyplot(AvgOfmean_age~ss|spp+as.factor(bin_int), tbldat, group=bin_n, subset=(Annual_mortality==0.2))
	}
	
if(n==6)
	{# table that gets 80% cv 80% of the time...
	tbldat<-sqlFetch(com3, "vbgf_2")
	tbldat[tbldat$AvgOfcv_bin<=0.8,]$ss<- NA 

	
	out<-ddply(tbldat,
		.(spp, bin_int, bin_n,Annual_mortality,type),
		summarise,
		n=length(ss),
		ss=min(na.omit(ss)))		
	out[out==Inf]<- NA	
	
	out2<- dcast(out,
		spp+ bin_int+ bin_n+Annual_mortality~type,
		value.var="ss", 
		fun=mean)
	
	write.csv(out2,"./tables/samplesize_vbgf.csv")
}
	
}