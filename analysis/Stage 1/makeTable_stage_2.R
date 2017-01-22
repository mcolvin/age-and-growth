# STAGE 2 TABLES
setwd("C:/Users/mcolvin/Documents/projects/Age and Growth/Analysis/Stage 1")
fish<- read.csv("fish.csv")
require(plyr)
require(reshape2)
library(data.table)
stage1<- read.csv("./output/stage_1_table.csv")[,-1]
stage1$k<- exp(1.2434+-0.66*log(stage1$linf))
stage1$t0<- 0	
stage1$maxAge<-round(log(1-(((stage1$linf*0.95)/stage1$linf)))/-stage1$k + stage1$t0)
	b = -1.01
	a = exp(1.46)
stage1$A<- 1-exp(-a*stage1$maxAge^b)
stage1$A<- round(stage1$A,4)
# ma
x<- fread("./output/ma_stage_2.csv",sep=",",header=TRUE)
x$A<- round(x$A,4)
x$val<-(abs(x$srs_ma-x$alk_ma)/x$srs_ma)*100
x<- merge(x,stage1[,c(1,2,7,8,9)],by=c("level","A"),all.x=TRUE)
xx<- as.data.frame(x[,j=list(val=mean(val,na.rm=TRUE),n=mean(n,na.rm=TRUE)),by=list(linf,bin,level,bin_n,ss)])

par(mfrow=c(2,2))
plot(val~bin_n, xx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xx, subset=level==5&linf==i &bin==10,col='red',type='l')}
plot(val~bin_n, xx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xx, subset=level==10&linf==i &bin==10,col='red',type='l')}	
plot(val~bin_n, xx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xx, subset=level==5&linf==i &bin==25,col='red',type='l')}		
plot(val~bin_n, xx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xx, subset=level==10&linf==i &bin==25,col='red',type='l')}		

xx$parm<- "ma"
out<-xx

# mla
x<- fread("./output/mla_stage_2.csv",sep=",",header=TRUE)
x$A<- round(x$A,4)
x$val<-(abs(x$mla_srs-x$mla_alk)/x$mla_srs)*100
x<- merge(x,stage1[,c(1,2,7,8,9)],by=c("level","A"),all.x=TRUE)
xx<- x[,j=list(val=mean(val,na.rm=TRUE),n=mean(n,na.rm=TRUE)),by=list(linf,bin,level,bin_n,ss,sim)]
xxx<- xx[,j=list(val=mean(val,na.rm=TRUE),n=mean(n,na.rm=TRUE)),by=list(linf,bin,level,bin_n,ss)]
xxx$parm<- "mla"
out<-rbind(out,xxx)
par(mfrow=c(2,2))
plot(val~bin_n, xxx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xxx, subset=level==5&linf==i &bin==10,col='red',type='l')}
plot(val~bin_n, xxx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xxx, subset=level==10&linf==i &bin==10,col='red',type='l')}	
plot(val~bin_n, xxx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xxx, subset=level==5&linf==i &bin==25,col='red',type='l')}		
plot(val~bin_n, xxx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xxx, subset=level==10&linf==i &bin==25,col='red',type='l')}		
## af
x<- fread("./output/af_stage_2.csv",sep=",",header=TRUE)
x$A<- round(x$A,4)
x$val<-(abs(x$p_srs-x$p_alk)/x$p_srs)*100
x<- merge(x,stage1[,c(1,2,7,8,9)],by=c("level","A"),all.x=TRUE)
xx<- x[,j=list(val=mean(val,na.rm=TRUE),n=mean(n,na.rm=TRUE)),by=list(linf,bin,level,bin_n,ss,sim)]
xxx<- xx[,j=list(val=mean(val,na.rm=TRUE),n=mean(n,na.rm=TRUE)),by=list(linf,bin,level,bin_n,ss)]
xxx$parm<- "af"
out<-rbind(out,xxx)
par(mfrow=c(2,2))
plot(val~bin_n, xxx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xxx, subset=level==5&linf==i &bin==10,col='red',type='l')}
plot(val~bin_n, xxx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xxx, subset=level==10&linf==i &bin==10,col='red',type='l')}	
plot(val~bin_n, xxx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xxx, subset=level==5&linf==i &bin==25,col='red',type='l')}		
plot(val~bin_n, xxx, type='n')
	for(i in seq(20,100,10)){points(val~bin_n, xxx, subset=level==10&linf==i &bin==25,col='red',type='l')}	


write.csv(out,"./output/stage_2.csv")  
