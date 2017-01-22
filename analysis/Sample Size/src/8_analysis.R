	# combos that are already done
	fn<- dir("./output")
	fn<- fn[!(fn%in%c("combos.csv","output.accdb",
		"output.laccdb","combos.csv","struc_all.csv","vbgf_all.csv", "struc_app.csv","vbgf_app.csv"))]
	fn1<- matrix(unlist(strsplit(fn,"_")),ncol=4,byrow=TRUE)
	fn<- data.frame(fp=paste("./output/",fn,sep=""),
		type=fn1[,2],
		combo=fn1[,3])
	ids<- unique(fn1[,3])
	combo_ids<- combos$combo
	combo_ids<- combo_ids[!(combo_ids %in% ids)]
	ddd<- matrix(combo_ids, ncol=1)
	
	
	
	sfInit(parallel=T, cpus=4)
	sfExportAll()
	sfLibrary(reshape2)
	sfLibrary(plyr)
	sfLibrary(nls2)
	sfClusterSetupRNG()
	result <- sfLapply(1:nrow(ddd), run_mc)
	sfStop()
			
			
			
			
			
	for(i in 1:nrow(ddd)) # loop over the models allocated to each core in fn
		{
		run_mc(w=i,node=1)
		}

	ddd<- matrix(combo_ids, ncol=4)	
	# SET UP FOR MULTICORE
	ncpus=4 # number of cores to use
	sfInit(parallel=T, cpus=ncpus)
	sfExportAll()
	sfLibrary(reshape2)
	sfLibrary(plyr)
	sfLibrary(nls2)
	sfClusterSetupRNG()
	# RUN THE FUNCTION run_mc on 4 cores
	sfLapply(1:ncpus, function(jj)# jj indexes the cores
		{
		for(i in 1:nrow(ddd)) # loop over the models allocated to each core in fn
			{
			run_mc(w = i, node=jj)
			print(i)
			print(jj)
			}
		})
		sfStop()
		# NOTE THERE WILL BE AN ERROR WHEN THE FUNCTION HITS THE NAs IN fn


setwd("C:/Users/mcolvin/Documents/projects/Age and Growth/Analysis/Sample Size")
	source("./src/1_global.R")  	
	source("./src/2_functions.R")         
	source("./src/3_load.R")     
  	source("./src/4_clean.R" )
 	source("./src/5_tables.R") 
	source("./src/6_figures.R") 	

for(i in 1:nrow(ddd)) # loop over the models allocated to each core in fn
{
run_mc(w = i, node=1)
}


