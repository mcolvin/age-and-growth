figures<- function(n){

if(n==1)
	{
	par(mfrow=c(2,2))
	boxplot(mean_len_prop~ss, struc_dat,
		ylim=c(-0.4,0.5))	
	panLab("a) mean length")
	boxplot(len_sim~ss, struc_dat,
		ylim=c(0,1.1))	
	panLab("b) size structure sim.")
	boxplot(mean_age_prop~ss, struc_dat,
		ylim=c(-0.7,0.8))
	panLab("c) mean age")
	boxplot(age_sim~ss, struc_dat,
		ylim=c(0,1.1))
	panLab("d) age structure sim.")
	}
if(n==2)
	{
	vbgf_dat$prop_bias<- (vbgf_dat$est-vbgf_dat$true)/vbgf_dat$true
	boxplot(prop_bias~ss,vbgf_dat, 
		subset=parm=="Linf")
	boxplot(prop_bias~ss,vbgf_dat, 
		subset=parm=="k")
	}
}