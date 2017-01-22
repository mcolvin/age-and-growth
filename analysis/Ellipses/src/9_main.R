
setwd("C:/Users/mcolvin/Documents/projects/Completed-Projects-And-Back-Burner/Age and Growth/Analysis/Ellipses/")
	source("./src/1_global.R")  	
	source("./src/2_functions.R")         
	source("./src/3_load.R")     
  	source("./src/4_clean.R" )
 	source("./src/5_tables.R") 
	source("./src/6_figures.R") 	
	source("./src/7_models.R")
	source("./src/8_analysis.R")
	
	# save combinations within the 95% ellipse
	write.csv(tables(2),"./output/ellipse.csv")
write.csv(fish_dat,"./output/ellipse-data.csv")