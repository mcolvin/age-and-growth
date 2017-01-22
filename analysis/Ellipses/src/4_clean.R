
	
#	Actinopterygii  ray finned fishes
#	Cephalaspidomorphi   jawless
#	Elasmobranchii  Shark rays, skates       
#	Holocephali chimeras


fish_dat<- subset(fish_dat, !(class %in% c("Myxini","Sarcopterygii")))
fish_dat$class<- factor(fish_dat$class)

# SUBSET JUST THE FIN RAYED FISHES W
fish_dat_fin_ray<- subset(fish_dat, class=="Actinopterygii")
fish_dat_fin_ray$order<- factor(fish_dat_fin_ray$order)
fish_dat_fin_ray$tmp<-1

