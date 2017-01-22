setwd("C:/Users/mcolvin/Documents/projects/Age and Growth/Analysis/Stage 1")
fish<- read.csv("fish.csv")
require(plyr)
require(reshape2)
library(data.table)

fish<- data.frame(linf=seq(20,100,10))
fish$species<-1:nrow(fish)
# mean age
out<- fread("./output/stage_1_ma.csv",drop=c(1)) 
out<-subset(out, ss<5000)
out<- merge(out,fish, by="species")
# CRITERIA
out$ap<- (abs(out$mn_age_true-out$srs_age_mn)/out$mn_age_true)*100
out$crit_1_ap<- ifelse(out$ap<=5,1,0)
out$crit_2_ap<- ifelse(out$ap<=10,1,0) 
x<-out[,j=list(crit_1_ap=mean(crit_1_ap),crit_2_ap=mean(crit_2_ap)),by=list(species,A,linf,ss)] 
x$crit_1_ss_ap<- ifelse(x$crit_1_ap<0.8, NA,x$ss)
x$crit_2_ss_ap<- ifelse(x$crit_2_ap<0.8,NA, x$ss)
aa<- as.data.frame(x[,j=list(ss1_ap=min(na.omit(crit_1_ss_ap)),ss2_ap=min(na.omit(crit_2_ss_ap))),by=list(species,linf,A)])
res<- aa	
res$parm<- "ma"

# mean length at age
out<- fread("./output/stage_1_mla.csv",drop=c(1)) 
out<- merge(out,fish, by="species")
out$ap<- (abs(out$mla_age_true-out$mla_age_srs)/out$mla_age_true)*100
out$ap_5<-out$ap
out$ap_10<-out$ap
out[is.na(out$mla_age_srs),]$ap_5<-5.1
out[is.na(out$mla_age_srs),]$ap_10<-10.1
xx<- out[,j=list(ap_5=mean(ap_5),ap_10=mean(ap_10)),by=list(species,A,linf,ss,sim)]
xx$crit_1_ap<- ifelse(xx$ap_5<=5,1,0)
xx$crit_2_ap<- ifelse(xx$ap_10<=10,1,0) 
xxx<-xx[,j=list(crit_1_ap=mean(crit_1_ap),crit_2_ap=mean(crit_2_ap)),by=list(species,A,linf,ss)] 
xxx$crit_1_ss_ap<- ifelse(xxx$crit_1_ap<0.8, NA,xxx$ss)
xxx$crit_2_ss_ap<- ifelse(xxx$crit_2_ap<0.8,NA, xxx$ss)
aa<- as.data.frame(xxx[,j=list(ss1_ap=min(na.omit(crit_1_ss_ap)),ss2_ap=min(na.omit(crit_2_ss_ap))),by=list(species,A,linf)])
aa$parm<- "mla"
res<- rbind(res,aa)


# age structure
out<- fread("./output/stage_1_af.csv",drop=c(1)) 
out<- merge(out,fish, by="species")
out$ap<- abs(out$mla_age_true-out$mla_age_srs)*100
out$ap_5<-out$ap
out$ap_10<-out$ap
out[is.na(out$mla_age_srs),]$ap_5<-1.1
out[is.na(out$mla_age_srs),]$ap_10<-2.1
xx<- out[,j=list(ap_5=mean(ap_5),ap_10=mean(ap_10)),by=list(species,A,linf,ss,sim)]
xx$crit_1_ap<- ifelse(xx$ap_5<=1,1,0)
xx$crit_2_ap<- ifelse(xx$ap_10<=2,1,0) 
xxx<-xx[,j=list(crit_1_ap=mean(crit_1_ap),crit_2_ap=mean(crit_2_ap)),by=list(species,A,linf,ss)] 
xxx$crit_1_ss_ap<- ifelse(xxx$crit_1_ap<0.8, NA,xxx$ss)
xxx$crit_2_ss_ap<- ifelse(xxx$crit_2_ap<0.8,NA, xxx$ss)
aa<- as.data.frame(xxx[,j=list(ss1_ap=min(na.omit(crit_1_ss_ap)),ss2_ap=min(na.omit(crit_2_ss_ap))),by=list(species,A,linf)])
aa$parm<- "af"
res<- rbind(res,aa)
res<- as.data.frame(res)

# full stage 1.
 tmp<- reshape(res,
  varying = names(res) [4:5],
  v.names = "ss",
  timevar= "type", 
  times = c("mape low","mape high"),
  direction = "long")
tmp$parm<- factor(tmp$parm, levels=c("ma","mla","af"))
tmp$level<-5
tmp[grep("high",tmp$type),]$level<-10
yyy<- dcast(tmp,level+linf~parm,value.var="ss",mean)
write.csv(tmp,"./output/xx.csv")
write.csv(yyy,"./output/stage_1_table.csv")






# 