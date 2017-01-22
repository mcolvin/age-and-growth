figures<- function(n){

if(n==1)
	{
	plot(log(K)~log(Linf), fish_dat)

}
if(n==2)
	{
	plot(log(K)~log(Linf),fish_dat,pch=19,cex=0.5,
		subset=class=="Actinopterygii")
	points(log(K)~log(Linf),fish_dat,pch=19,cex=0.5,
		subset=class=="Cephalaspidomorphi", col="red")
	points(log(K)~log(Linf),fish_dat,pch=19,cex=0.5,
		subset=class=="Elasmobranchii", col="green")
	points(log(K)~log(Linf),fish_dat,pch=19,cex=0.5,
		subset=class=="Holocephali", col='blue')
	}
if(n==3)
	{# 95% CI by fish class
	dataEllipse(log(fish_dat$Linf), 
		log(fish_dat$K),
		groups=fish_dat$class,  
		pch='.',
		xlim=c(0, 7), center.pch="+",
		ylim=c(-5,3), level=.95, fill=TRUE, 
		fill.alpha=0.1,
		xlab="Length at infinity; log scale",
		ylab="Brody growth coefficient; log scale")
	}
if(n==4)
	{# 95% CI for all fish
	dataEllipse(log(fish_dat$Linf), 
		log(fish_dat$K), 
		levels=c(0.90, 0.975))
	}
if(n==6)
	{# PLOT OF GRID LINF AND K VALUES
	dataEllipse(log(fish_dat_fin_ray$Linf), 
		log(fish_dat_fin_ray$K),
		pch='.', xlim=c(0, 7), 
		center.pch="+",draw=FALSE,
		ylim=c(-5,3), level=.95, 
		fill=TRUE, fill.alpha=0.1)
	out<- tables(2)
	points(Var2~Var1, out, subset=pip==1,pch=19)

	}
if(n==7)
	{
	
ords<-subset(ords, tmp>=10)
fish_dat<- subset(fish_dat, order %in% ords$order)
fish_dat$order<- factor(fish_dat$order)

yy<-with(fish_dat, dataEllipse(log(Linf), log(K),
	order,  
    xlim=c(1, 7), center.pch="+",
    ylim=c(-5,3), level=.95, fill=TRUE, fill.alpha=0.09))	

	}
}