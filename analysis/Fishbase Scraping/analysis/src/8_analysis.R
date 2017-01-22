



## GROWTH DATA
for(i in 1:nrow(yyy))
	{
	urll<- as.character(paste("http://www.fishbase.ca/PopDyn/PopGrowthList.php?ID=",yyy$id[i],sep=""))
	repeat
		{
		x<- readLines(urll,warn=FALSE)
		tmp<-length(grep("MySQL database is down at the moment" ,x))+
			length(grep("ERROR: Cannot connect" ,x))+	
			length(grep("Cannot Connect to database" ,x))				
		if(tmp==0){break}	
		if(length(tmp)>0) {Sys.sleep(20)}
		}
	tmp<-grep("The system found no growth information for the requested specie",x)
	if(length(tmp)!=0) {yyy$done[i]<-2}
	if(length(tmp)==0) 
		{
		# PARSE OUT DATA
		xxx<- x
		xxx<-xxx[-c(1:(match("\t\t\t\t<th>Loo<br/>(cm)</th>" ,xxx)-1))]
		headers<- xxx[grep("<th>",xxx)]
		headers<- gsub("\t","",headers)
		headers<- gsub("<th>","",headers)
		headers<- gsub("</th>","",headers)
		headers<- gsub("<br>","",headers)
		headers<- gsub("<br/>"," ",headers)

		dat<-xxx[grep("</td>",xxx)]
		dat<-dat[-c(grep('<input type=' ,dat))]
		dat<- gsub("\t","",dat)
		dat<- gsub("<td>","",dat)
		dat<- gsub("</td>","",dat)
		dat<-dat[-length(dat)]
		y<-matrix(dat,ncol=length(headers),byrow=TRUE)
		y<- as.data.frame(y)
		names(y)<- headers
		fn<- paste("./dat/",yyy$name[i],"__",yyy$id[i],".csv",sep="")
		write.csv(y, fn)
		yyy$done[i]<-1
		write.csv(yyy,"./dat/yyy.csv")
		}
	print(i)
	}



for(i in 1:nrow(yyy))
	{
	urll<- as.character(paste("http://www.fishbase.org/PopDyn/PopCharList.php?ID=",yyy$id[i],sep=""))
	repeat
		{
		x<- readLines(urll,warn=FALSE)
		tmp<-length(grep("MySQL database is down at the moment" ,x))+
			length(grep("ERROR: Cannot connect" ,x))+	
			length(grep("Cannot Connect to database" ,x))				
		if(tmp==0){break}	
		if(length(tmp)>0) {Sys.sleep(20)}
		}
	tmp<-grep("ishBase is unable to find data related to List of Population Characteristics records for",x)
	if(length(tmp)!=0) {yyy$done[i]<-2}
	if(length(tmp)==0) 
		{
		# PARSE OUT DATA
		xxx<- x
		xxx<-xxx[-c(1:(grep('Sex' ,xxx)-1))]
		headers<- xxx[grep("<th",xxx)]
		headers<- gsub("\t","",headers)
		headers<- gsub("<th>","",headers)
		headers<- gsub("</th>","",headers)
		headers<- gsub("<br>","",headers)
		headers<- gsub("<br/>"," ",headers)
		headers<-matrix(unlist(strsplit(headers,">")),ncol=2, byrow=TRUE)[,2]
		dat<-xxx[grep("</td>",xxx)]
		dat<- gsub("\t","",dat)
		dat<- gsub("<td>","",dat)
		dat<- gsub("</td>","",dat)
		dat<- gsub("</a>","",dat)
		dat<- gsub('<td class=\"lalign\">',"",dat)
		
		y<-matrix(dat,ncol=length(headers),byrow=TRUE)		
		y[,1]<- matrix(unlist(strsplit(y[,1],">")),ncol=2, byrow=TRUE)[,2]
		y<- as.data.frame(y)
		names(y)<- headers
		fn<- paste("./dat/age_size_",yyy$name[i],"__",yyy$id[i],".csv",sep="")
		write.csv(y, fn)
		yyy$done[i]<-1
		write.csv(yyy, "./dat/age_size.csv")
		}
	print(i/nrow(yyy)*100)
	}







for(i in 2:length(fn))
	{# read in individual files and appedn spp and sppid
	app<- read.csv(paste("./dat/",fn[i],sep=""))
	app$spp<- strsplit(fn[i],"__")[[1]][1]
	app$sppid<- strsplit(fn[i],"__")[[1]][2]
	app$sppid<- as.numeric(gsub(".csv","",app$sppid))
	dat<- rbind(dat, app)
	}
# post processing of k
dat[,4]<-gsub("</a>",""	,dat[,4])
dat[,4]<-gsub("<a",""	,dat[,4])
dat[,4]<-  as.numeric(matrix(unlist(strsplit(dat[,4],">")),ncol=2, byrow=TRUE)[,2])
dat<- dat[,-1]
names(dat)<- c("Linf", "length_type","K","t0","sex","temp","Lm","","omega","country","locality", "questionable", "captive")
dat$Linf[dat$Linf==0]<-NA
dat$K[dat$K==0]<-NA
dat$Linf<- as.numeric(gsub(",","",dat$Linf))

# write the file out
write.csv(dat,"./dat/full_dat.csv")




	
# tmax data
# a script to complile vbgf output
dat<- read.csv(paste("./dat/",fn_vbgf[1],sep=""))
dat$spp<- strsplit(fn_vbgf[1],"__")[[1]][1]
dat$sppid<- strsplit(fn_vbgf[1],"__")[[1]][2]
dat$sppid<- as.numeric(gsub(".csv","",dat$sppid))

for(i in 2:length(fn_vbgf))
	{# read in individual files and appedn spp and sppid
	app<- read.csv(paste("./dat/",fn_vbgf[i],sep=""))
	app$spp<- strsplit(fn_vbgf[i],"__")[[1]][1]
	app$sppid<- strsplit(fn_vbgf[i],"__")[[1]][2]
	app$sppid<- as.numeric(gsub(".csv","",app$sppid))
	dat<- rbind(dat, app)
	}

# write the file out
write.csv(dat,"./dat/age_size_full_dat.csv")




