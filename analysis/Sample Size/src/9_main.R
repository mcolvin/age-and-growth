# to do
#1. make a function that updates vbgf and structure 
#such that it I can compile and append data opposed to 
#cranking through all the csv's...



setwd("C:/Users/mcolvin/Documents/projects/Age and Growth/Analysis/Sample Size")
	source("./src/1_global.R")  	
	source("./src/2_functions.R")         
	source("./src/3_load.R")     
  	source("./src/4_clean.R" )
 	source("./src/5_tables.R") 
	source("./src/6_figures.R") 	
	
	source("./src/7_analysis.R")
	
	
	# table of combinations
	# need to subset out files that are
	# alread in the dbase
	#write.csv(tables(1),"./output/combos.csv")
	write.csv(tables(2),"./output/vbgf_app.csv")# makes a csv to be appended to exising dbase
	write.csv(tables(3),"./output/struc_app.csv")
