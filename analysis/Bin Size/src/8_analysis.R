	## SIMULATE LENGTH AT AGE ASSUMING A VBGF
	linf=300
	k=0.4
	t0=0.0
	cv_vbgf=0.1	
	# ASSIGN TRUE LENGTH AND OBSERVED LENGTH, LENGTH TO THE NEARST MM
	ss<- c(50,100,250,500,750,1000,1250)
	bin_int<- c(10,25,50)
	bin_n<- 10
	# interval<- 0.5*round(linf/30/0.5)# round to nearest 5 mm
	combos<- expand.grid(ss=ss,bin_int=bin_int,
		linf=linf,k=k,t0=t0,cv_vbgf=cv_vbgf)
	out_srs<-out_bin<- data.frame()

for(h in 5:nrow(combos))
	{
	# SIMULATE POPULATION GIVEN INPUT PARAMETERS
	dat<- generate_data(
		nsims=1000,
		A=8,
		a_mat=2,
		sigma_rec=0.3,
		r_median=10000,
		F_median=0.2,
		sigma_F=0.1)
	dat$mn_length<- linf * (1 - exp(-k * (dat$age-t0)))

	for(i in 1:max(dat$sim))
		{
		# EXPAND CATCH COUNT DATA
		catch<- as.data.frame(lapply(dat[dat$sim==i,],
                   function(x) rep(x,dat[dat$sim==i,]$C)))
		# ASSIGN LENGTH TO EXPANDED DATASET
		catch$len_true<- rnorm(length(catch$mn_length),catch$mn_length,catch$mn_length*combos$cv_vbgf[h])
		catch$len_obs<- round(catch$len_true,0)

		# BIN THE TRUE DATA 
		brks<- seq(0, (max(catch$len_obs)+bin_int[1]),bin_int[1])
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
		
		# RANDOM SAMPLE ANALYSIS
		samp<- subset(catch, srs<=combos$ss[h])		
		
		## FIT VBGF TO BINNED DATA
		fit<- try(
			nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
			samp,
			start=list(
				Linf=combos$linf[h], 
				k=combos$k[h], 
				t0=combos$t0[h]),
			control=list(maxiter=3000)),
			silent=T)
		if(class(fit)=="try-error") 
			{
			# BRUTE FORCE FOR NLS STARTS
			starts<- data.frame(Linf=runif(2500,0.7*combos$linf[h],1.2*combos$linf[h]),
				k=runif(2500,0.7*combos$k[h],1.2*combos$k[h]),
				t0=runif(2500,1.2*combos$t0[h],0.7*combos$t0[h]))
			mod1 <- nls2(len_obs~ Linf * (1- exp(-k*(age-t0))),samp,  
				start = starts,
				algorithm = "brute-force")
			fit<- try(nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
				samp,start =  coef(mod1),
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
		app_srs<- data.frame(combos=h,sim=i, parameter = rownames(y), est=y[,1], se=y[,2],cv=y[,2]/y[,1])
		out_srs<- rbind(out_srs, app_srs)
		# END 

		
		# BINNING ANALYSIS
		
		## SELECT BINNED SAMPLE AS THE TOTAL SAMPLE SIZE LESS THOSE THAT ARE OVER THE BIN
		samp<- subset(catch, bin<=combos$bin_n[h])	
		samp<- samp[order(samp$srs),]
		samp<- samp[c(1:combos$ss[h]),] # fixes bin and keeps sample size the same
		samp<- subset(samp,bin<=combos$bin_n[h])
	
		## FIT VBGF TO BINNED DATA
		fit<- try(
			nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
			samp,
			start=list(
				Linf=combos$linf[h], 
				k=combos$k[h], 
				t0=combos$t0[h]),
			control=list(maxiter=3000)),
			silent=T)
	
		if(class(fit)=="try-error") 
			{
			# BRUTE FORCE FOR NLS STARTS
			starts<- data.frame(Linf=runif(2500,0.7*combos$linf[h],1.2*combos$linf[h]),
				k=runif(2500,0.7*combos$k[h],1.2*combos$k[h]),
				t0=runif(2500,1.2*combos$t0[h],0.7*combos$t0[h]))
			mod1 <- nls2(len_obs~ Linf * (1- exp(-k*(age-t0))),samp,  
				start = starts,
				algorithm = "brute-force")
			fit<- try(nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
				samp,start =  coef(mod1),
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
		app_bin<- data.frame(combos=h,sim=i, parameter = rownames(y), est=y[,1], se=y[,2],cv=y[,2]/y[,1])
		out_bin<- rbind(out_bin, app_bin)
		# END
	} # end i
write.csv(out_srs,"./output/out_srs.csv")	
write.csv(out_bin,"./output/out_bin.csv")
print(paste(round(h/nrow(combos)*100,2),"% done"))
}# end h











# THIS SCRIPT EVALUATES THE 
# EFFECT OF 'BINNING' AS A 
# STRATEGY TO ESTIMATE VBGF





# Simulate the age structure

set.seed(111)
Z<- 0.25
A<- 1-exp(-Z)
N<- 10000
for(i in 2:11)
  {
	N<- c(N, N[i-1]-N[i-1]*A)	
	}
linf<-50.5
k<- 0.31
t0<-0.41
er<- 0.1
true<- data.frame(age=rep(c(1:11),N))
true$l<- linf * (1 - exp(-k * (true$age-t0)))*
  rlnorm(nrow(true),0,er)

plot(l~jitter(age),true,las=1, ylab="Length (cm)", xlab="Age")

true<- subset(true, age>1)
simDat_srs<- data.frame()
simDat_bin<- data.frame()
for(i in 1:40)
  {
  indx<- sample(c(1:nrow(true)),30,replace=FALSE)
  simDat_srs<- rbind(simDat_srs, true[indx,]) 
  }
  

  
  
ll<- function(p,dat)
  {
  pred<-p[1] * (1 - exp(-p[2] * (dat$age-p[3])))
  pred<- ifelse(pred<=0,0.0001, pred) 
  logl<- dlnorm(dat$l,log(pred),exp(p[4]), log=TRUE)
  return(-sum(logl))
  }
  
# BIN THE TRUE DATA 
interval<- 0.5*round(linf/30/0.5)# round to nearest 5 mm
brks<- seq(min(true$l), max(true$l),interval)
labs<- c(1:(length(brks)-1))
true$bin<- cut(true$l, breaks=brks, labels=labs)

# SIMPLE RANDOM SAMPLE SIZES TO FILL BIN CELLS
total<- c(200,500,1000, 1500)

out_pest<- data.frame()
for(i in 1:100)
	{
	# ONE REP FOR ALL SRS SAMPLE SIZES
	indx<- sample(c(1:nrow(true)),nrow(true),replace=FALSE)
	samp<- data.frame(n= rep(total,total),
	indx=c(indx[1:total[1]],indx[1:total[2]],indx[1:total[3]],indx[1:total[4]]))
	# NOW FILL TO SAMPLE TO GET TO BINS
	srs<- true[samp[samp$n==200,]$indx,]
	srs$id<- c(1:nrow(srs))
	srs_bins<- list()
	# SUBSAMPLE TO FILL BINS, IF POSSIBLE
	out<- unlist(sapply(1:length(labs),function(x) {head(srs[srs$bin==x,]$id,10)}))
	# NEW DATASET WITH ONLY 10, AT MOST PER BIN
	srs<- subset(srs, id%in% out)
	
	# NOW WE CAN FIT A VBGF TO THE DATA
	ini<- c(linf, k, t0,log(0.1))
	fit<-NA
	fit<- optim(ini, ll, method="BFGS",hessian=TRUE, dat=srs)
	if(is.na(fit[1])){outp<- data.frame(type='bin',rep=i, p=c("linf","k","t0","er"),est=-99, se= -99,obs=-99)} 
	if(is.na(fit[1])==FALSE){outp<- data.frame(type='bin',rep=i, p=c("linf","k","t0","er"),est=fit$par, 
		se= sqrt(diag(solve(fit$hessian))),obs=nrow(srs))}
	out_pest<- rbind(out_pest,outp)
	}

# SAME AS ABOVE BUT FOR SRS NO BINNING AND N = 180
for(i in 1:100)
	{
	# ONE REP FOR ALL SRS SAMPLE SIZES
	indx<- sample(c(1:nrow(true)),100,replace=FALSE)
	srs<- true[indx,]
	# NOW WE CAN FIT A VBGF TO THE DATA
	ini<- c(linf, k, t0,log(0.1))
	fit<-NA
	fit<- optim(ini, ll, method="BFGS",hessian=TRUE, dat=srs)
	if(is.na(fit[1])){outp<- data.frame(type='srs',rep=i, p=c("linf","k","t0","er"),est=-99, se= -99,obs=-99)} 
	if(is.na(fit[1])==FALSE){outp<- data.frame(type='srs',rep=i, p=c("linf","k","t0","er"),est=fit$par, 
		se= sqrt(diag(solve(fit$hessian))),obs=nrow(srs))}
	out_pest<- rbind(out_pest,outp)
	}

	out_pest[out_pest$p=="er",]$est<- exp(out_pest[out_pest$p=="er",]$est)
	out_pest$true<-NA
	out_pest[out_pest$p=="linf",]$true<-linf
	out_pest[out_pest$p=="er",]$true<-0.1
	out_pest[out_pest$p=="k",]$true<-k
	out_pest[out_pest$p=="t0",]$true<-t0
	out_pest$bias<- (out_pest$est-out_pest$true)/out_pest$true
	
	
boxplot(bias~p,out_pest,at=c(1:4)-0.25,subset=(type=="bin"),boxwex=0.25)
boxplot(bias~p,out_pest,at=c(1:4)+0.25,subset=(type=="srs"),boxwex=0.25,add=TRUE,col="red")
abline(h=0)


boxplot(log(se)~p,out_pest,at=c(1:4)-0.25,subset=(type=="bin"),boxwex=0.25)
boxplot(log(se)~p,out_pest,at=c(1:4)+0.25,subset=(type=="srs"),boxwex=0.25,add=TRUE,col="red")
abline(h=0)

boxplot(se~p,out_pest,at=c(1:4)-0.25,subset=(type=="bin"),boxwex=0.25)
boxplot(se~p,out_pest,at=c(1:4)+0.25,subset=(type=="srs"),boxwex=0.25,add=TRUE,col="red")
abline(h=0)	






	
	##### SRS: randomly select  individuals
	srs_samples<-data.frame()
	for(i in 1:nsim)
		{
		xx<- subset(dat, rep==i)
		app<- xx[sample(c(1:pop),
			ssize, 
			replace=FALSE),]
		srs_samples<- rbind(srs_samples,app)				
		}
			
		# REFERENCE DATA
		## MEAN AGE
		out<- ddply(dat,"rep",summarise,
			meanAge= mean(age),
			maxAge=max(age))
		out$metaId<-h	
	 	# SAVE TRUE MEAN AND MAXIMUM AGE IN RESULTS LIST
		res$age_true<- out
		## AGE STRUCTURE
		out<- dcast(dat,rep~age,value.var="age",drop=F, fill=0,length)
		out<- cbind(out[,1],out[,-1]/pop)
		names(out)[1]<-c("rep")
		out$metaId<-h			
		res$age_structure_true<- out
			
		# SAMPLING DATA
		## MEAN AGE
		out<- ddply(srs_samples,"rep",summarise,
			meanAge= mean(age),
			maxAge=max(age))
		out$metaId<-h
	 	# SAVE TRUE MEAN AND MAXIMUM AGE IN RESULTS LIST
		res$age_srs<- out
		
		## AGE STRUCTURE
		y<- dcast(srs_samples,rep~age,value.var="age",drop=F, fill=0,length)
		y<- cbind(y[,1],y[,-1]/pop)
		names(y)[1]<-c("rep")
		y$metaId<-h
		res$age_structure_srs<- y		

		## GROWTH CURVE 
		out<- data.frame()
		for(i in 1:nsim)
			{
			fit<- try(
				nls(L~ Linf * (1- exp(-k*(age-t0))),
				srs_samples,subset=rep==i,
				start=list(
					Linf=Linf, 
					k=k, 
					t0=t0),
				control=list(maxiter=3000)),
				silent=T)
	
			if(class(fit)=="try-error") 
				{
				# BRUTE FORCE FOR NLS STARTS
				starts<- data.frame(Linf=runif(2500,0.7*Linf,1.2*Linf),
					k=runif(2500,0.7*k,1.2*k),
					t0=runif(2500,1.2*t0,0.7*t0))
				mod1 <- nls2(L~ Linf * (1- exp(-k*(age-t0))),srs_samples,  
					start = starts,
					algorithm = "brute-force",
					subset=rep==rep)
				fit<- try(nls(L~ Linf * (1- exp(-k*(age-t0))),
					srs_samples,subset=rep==rep,
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
			app<- data.frame(metaId=h,rep=i, parameter = rownames(y), est=y[,1], se=y[,2],cv=y[,2]/y[,1])
			out<- rbind(out, app)
			}
			out$true<- NA
			out[out$parameter=="Linf",]$true<- Linf
			out[out$parameter=="k",]$true<- k
			out[out$parameter=="t0",]$true<- t0
			res$growthcurve<- out			
		##### END SIMPLE RANDOM SAMPLING SRS

		##### CLUSTER SAMPLING WITHIN 5 EQUAL INTERVALS BETWEEN 0 AND Linf
		interval_samples<-data.frame()
		# SET UP INTERVALS
		
		
		for(i in 1:nsim)
			{
			xx<- subset(dat, rep==i)
			app<- xx[sample(c(1:pop),
				ssize, 
				replace=FALSE),]
			interval_samples<- rbind(srs_samples,app)				
			}	
			
			
			}


	
# evaluate bias
true<- subset(growthcurve_sim, rep==0 & ssize==20)
names(true)[3]<- "true"
sim<- subset(growthcurve_sim, rep>0)
sim<- merge(sim, true[,c(2,3)],by="parameter")
sim$bias<- sim$est-sim$true
figures(1)

sim$cv_bin<- ifelse(sim$cv<=0.2, 1, 0) 
xx<- dcast(sim, ssize~"prop_cv", value.var="cv_bin", mean)
plot(prop_cv~ssize, xx)

xx<- dcast(sim, ssize~"prop_cv", value.var="cv_bin", 
	subset=.(parameter=="Linf"),mean)
plot(prop_cv~ssize, xx)

xx<- dcast(sim, ssize~"prop_cv", value.var="cv_bin", 
	subset=.(parameter=="k"),mean)
plot(prop_cv~ssize, xx)
	
xx<- dcast(sim, ssize~"prop_cv", value.var="cv_bin", 
	subset=.(parameter=="t0"),mean)
plot(prop_cv~ssize, xx)	


	