




hist(fish_dat_fin_ray$Linf)


fit<- lm(log(K)~log(Linf), fish_dat_fin_ray,subset=length_type=='TL' & country %in%c('Cada','Canada','Mexico','USA'))
		
		
		
		
plot(log(K)~log(Linf), fish_dat_fin_ray,subset=length_type=='TL' & country %in%c('Cada','Mexico','USA'))
abline(coef(fit)[1],coef(fit)[2],col="lightgrey", lwd=4)

x<- c(seq(20,100, 10),21)
pred<- predict(fit, data.frame(Linf=x))
exp(pred)
points(pred~log(x),col="red",pch=19,cex=2)

plot(K~Linf, fish_dat_fin_ray,subset=Linf>=20 & Linf<100)
abline(coef(fit)[1],coef(fit)[2],col="lightgrey", lwd=4)

vbgf(210,0.
