tables<- function(n){

if(n==1)
	{
	ords<- aggregate(tmp~order, fish_dat, sum)
	#write.csv(ords,"./tables/orders_count.csv")
	}
if(n==2)
	{# make a table of values that are in the 95% confidence ellipse
	# [1] GET THE 95% CI ELLIPSE
	y<- dataEllipse(log(fish_dat_fin_ray$Linf), 
		log(fish_dat_fin_ray$K),
		pch='.', xlim=c(0, 7), 
		center.pch="+",draw=FALSE,
		ylim=c(-5,3), level=.95,segments=200, 
		fill=TRUE, fill.alpha=0.1)
	
	# CREATE A RECTANGLE AROUND THE ELLIPES
	xx<- seq(1,6, 0.1) # POSSIBLE LINF VALUES
	yy<- seq(-4,2, 0.1) # POSSIBLE K VALUES
	combos<- expand.grid(xx,yy)
	
	# [2] EXTRACT THE 95% CI ellipse  combos 
	#     of linf and k!!! to feed the loop
	# AS THOSE VALUES IN THE 95% ELLIPSE
	out <- pnt.in.poly(combos,y)
	out<- subset(out,pip==1)
	return(out)
	}
}