


	fn<- fn[!(fn%in%c("combos.csv","output.accdb",
		"output.laccdb","combos.csv","struc_all.csv","vbgf_all.csv",
		"struc_app.csv","vbgf_app.csv"))]
	fn1<- matrix(unlist(strsplit(fn,"_")),ncol=4,byrow=TRUE)
	fn<- data.frame(fp=paste("./output/",fn,sep=""),
		type=fn1[,2],
		combo=fn1[,3])





	
	
	
	