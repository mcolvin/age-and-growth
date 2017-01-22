# MEAN AGE: STAGE 2	
require(plyr)
require(reshape2)
library(FSA)
setwd("C:/Users/mcolvin/Documents/projects/Age and Growth/Analysis/Stage 1")
fish<- read.csv("fish.csv")

stage1<- read.csv("./output/20150615_stage_1_table_v2.csv")[,-1]
fish<- merge(stage1,fish[,-1], by=c("spp","Asim_f"),all.x=TRUE)
nsims=1000
bin_n<-c(5,10,15,20,25,30,35,40,45,50)
out<- data.frame()
bin<- c(10,25)

# MEAN AGE: STAGE 1
out<- data.frame()
for(kk in c(9:16))
	{
	N<- fish$N[kk]
	Linf<- fish$linf[kk]#21.5
	k<- fish$K[kk]#0.45
	t0<- fish$t0[kk]#-0.313
	maxAge<- fish$tmax[kk]# 6
	spp<- fish$spp[kk]#"blg"
	species<- fish$species[kk]#"blg"
	A<- fish$Asim[kk]
	A_f<- fish$Asim_f[kk]
	ss_af<- fish$af[kk] 
	ss_ma<- fish$ma[kk]
	ss_mla<- fish$mla[kk]
	mape<-fish$level[kk]
	survival<- cumprod(rep(1-A,maxAge))
	dat<-data.frame(age=c(1:maxAge),N=rmultinom(1,N,survival))
	dat$exp_length<- Linf  * (1 - exp(-k * (dat$age-t0))) *10 # in mm  
	pop<- as.data.frame(lapply(dat,function(x) rep(x,dat$N)))
	pop$age_f<- as.factor(pop$age)
	# ASSIGN LENGTH TO EXPANDED DATASET
	pop$len_true<- rnorm(length(pop$exp_length),pop$exp_length,pop$exp_length*0.2)
	pop$len_obs<- round(pop$len_true,1)
	pop<- subset(pop,len_true>0)
	# loop over bins
	for(n in c(1:2))
		{			
		bins<- seq(0,(max(pop$len_obs)+100),bin[n])
		bin_func<- approxfun(bins,bins, method="constant")
		pop$l_bin<- bin_func(pop$len_obs)	
		pop$l_bin<-factor(pop$l_bin,levels=bins)
		# loop over bin n
		for(ll in c(1:length(bin_n)))
			{
			for(i in c(1:nsims))
				{
				## MEAN AGE
				### SRS LEVEL
				srs_indx_ma<- sample(c(1:nrow(pop)),ss_ma,replace=FALSE)
				srs_ma<- pop[srs_indx_ma,]
				srs_ma_out<- mean(srs_ma$age)
				srs_ma$bin_indx<- 1
				srs_ma<- srs_ma[order(srs_ma$len_obs),]
				x<-aggregate(bin_indx~l_bin,srs_ma,sum)
				srs_ma$bin_indx<- unlist(sapply(1:nrow(x),
					function(i){return(sample(c(1:x$bin_indx[i]),x$bin_indx[i],replace=FALSE))}))	
				alk_ma<- subset(srs_ma,bin_indx<=bin_n[ll])
				len.n <- xtabs(~l_bin,data=srs_ma)
				raw <- xtabs(~l_bin+age_f,data=alk_ma)
				key <- prop.table(raw, margin=1)
				key[is.na(key)]<-0
				srs_age_struc_hat<- prop.table(t(key) %*% table(srs_ma$l_bin))
				srs_age_struc_hat<-data.frame(age=as.numeric(rownames(srs_age_struc_hat)),  struc=srs_age_struc_hat)				
				srs_mean_age_hat<- sum(srs_age_struc_hat$struc*srs_age_struc_hat$age)
				stage_2_ma<- data.frame(srs_ma=srs_ma_out,alk_ma=srs_mean_age_hat)
				stage_2_ma$species=species
				stage_2_ma$spp=spp
				stage_2_ma$A=A
				stage_2_ma$A_f=A_f
				stage_2_ma$ss=ss_ma
				stage_2_ma$sim=i
				stage_2_ma$n=nrow(alk_ma)
				stage_2_ma$level=mape
				stage_2_ma$bin=bin[n]
				stage_2_ma$bin_n=bin_n[ll]				
				if("ma_stage_2_2_of_4.csv" %in% dir("./output")==FALSE)
					{write.csv(stage_2_ma,"./output/ma_stage_2_2_of_4.csv")} else
					{write.table(stage_2_ma,"./output/ma_stage_2_2_of_4.csv",append=TRUE, col.names=FALSE,sep=',')}		
				
				## AGE FREQUENCY
				### SRS LEVEL
				srs_indx_af<- sample(c(1:nrow(pop)),ss_af,replace=FALSE)
				srs_af<- pop[srs_indx_af,]
				srs_af_out<- dcast(srs_af,age_f+age~"p_srs",value.var="len_obs",length)
				srs_af_out$p_srs<- srs_af_out$p/sum(srs_af_out$p_srs)
				srs_af$bin_indx<- 1
				srs_af<- srs_af[order(srs_af$len_obs),]
				x<-aggregate(bin_indx~l_bin,srs_af,sum)
				srs_af$bin_indx<- unlist(sapply(1:nrow(x),
					function(i){return(sample(c(1:x$bin_indx[i]),x$bin_indx[i],replace=FALSE))}))	
				alk_af<- subset(srs_af,bin_indx<=bin_n[ll])
				alk_af_out<- dcast(alk_af,age_f+age~"p_alk",value.var="len_obs",length)
				alk_af_out$p_alk<- alk_af_out$p_alk/sum(alk_af_out$p_alk)				
				stage_2_af<- merge(srs_af_out,alk_af_out, by=c("age_f","age"),all.x=TRUE)
				stage_2_af$species=species
				stage_2_af$spp=spp
				stage_2_af$A=A
				stage_2_af$A_f=A_f
				stage_2_af$ss=ss_af
				stage_2_af$sim=i
				stage_2_af$n=nrow(alk_af)
				stage_2_af$level=mape
				stage_2_af$bin=bin[n]
				stage_2_af$bin_n=bin_n[ll]
				if("af_stage_2_2_of_4.csv" %in% dir("./output")==FALSE)
					{write.csv(stage_2_af,"./output/af_stage_2_2_of_4.csv")} else
					{write.table(stage_2_af,"./output/af_stage_2_2_of_4.csv",append=TRUE, col.names=FALSE,sep=',')}
				
				## MEAN LENGTH AT AGE
				### SRS LEVEL
				srs_indx_mla<- sample(c(1:nrow(pop)),ss_mla,replace=FALSE)
				srs_mla<- pop[srs_indx_mla,]
				srs_mla_out<- dcast(srs_mla,age_f+age~"mla_srs",value.var="len_obs",mean)	
				len.n <- xtabs(~l_bin,data=srs_mla)
				srs_mla$bin_indx<- 1
				srs_mla<- srs_mla[order(srs_mla$len_obs),]
				x<-aggregate(bin_indx~l_bin,srs_mla,sum)
				srs_mla$bin_indx<- unlist(sapply(1:nrow(x),
					function(i){return(sample(c(1:x$bin_indx[i]),x$bin_indx[i],replace=FALSE))}))	
				alk_mla<- subset(srs_mla,bin_indx<=bin_n[ll])
				raw <- xtabs(~l_bin+age_f,data=alk_mla)
				key <- prop.table(raw, margin=1)
				alk_mla_out<- suppressWarnings(alkMeanVar(key,len_obs~l_bin+age_f,alk_mla,len.n)[,c(1:2)])
				names(alk_mla_out)[2]<-"mla_alk"
				stage_2_mla<-merge(srs_mla_out,alk_mla_out, by=c("age"),all.x=TRUE)
				stage_2_mla$species=species
				stage_2_mla$spp=spp
				stage_2_mla$A=A
				stage_2_mla$A_f=A_f
				stage_2_mla$ss=ss_mla
				stage_2_mla$sim=i
				stage_2_mla$n=nrow(alk_mla)
				stage_2_mla$level=mape
				stage_2_mla$bin=bin[n]
				stage_2_mla$bin_n=bin_n[ll]
				if("mla_stage_2_2_of_4.csv" %in% dir("./output")==FALSE)
					{write.csv(stage_2_mla,"./output/mla_stage_2_2_of_4.csv")} else
					{write.table(stage_2_mla,"./output/mla_stage_2_2_of_4.csv",append=TRUE, col.names=FALSE,sep=',')}						
				}# i
			}# ll
		} #n
	} # kk
