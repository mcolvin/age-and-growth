



names(dat)[c(15:16)]<- c("spp", "sppid")
dat<- dat[,-1]
data(fishbase)
# table of taxa data to link to Linf and k data
taxa<- data.frame(
	genus=unlist(lapply(fish.data, function(x) x[1])),
	family= unlist(lapply(fish.data, function(x) x[2])),
	ScientificName= unlist(lapply(fish.data, function(x) x[3])),
	class=unlist(lapply(fish.data, function(x) x[10])),
	order=unlist(lapply(fish.data, function(x) x[13])),
	sppid=unlist(lapply(fish.data, function(x) x[14])))
dat_full<- merge(dat,taxa, by="sppid",all.x=TRUE)
	







xx<- ddply(dat_age_size, c("sppid"), summarise,
              # N    = length(Tmax..y.),
               mean_tmax = mean(Tmax..y.),
               med_tmax = median(Tmax..y.),
               max_tmax = max(Tmax..y.),

               #mean_wmax = mean(Wmax),
               #med_wmax = median(Wmax),
               #max_wmax = max(Wmax),			   

               mean_lmax = mean(Lmax..cm.),
               med_lmax = median(Lmax..cm.),
               max_lmax = max(Lmax..cm.))

datxx<- merge(dat_full,xx, by.x="id",by.y="sppid", all.x=TRUE)
names(datxx)
datxx<- datxx[,-c(2,10)]
datxx[datxx$sex%in%c("","FALSE"),]$sex<- NA
datxx$sex<- factor(datxx$sex)
levels(datxx$sex)

write.csv(datxx, "./dat/all.csv")
write.csv(datxx, "./dat/all_sas.csv",na=".")
