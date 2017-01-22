figures<- function(n){

if(n==1)
	{
	plot(log(K)~log(Linf),dat,type="n")
	points(log(K)~log(Linf),dat,subset=length_type=="NG")
	points(log(K)~log(Linf),dat,subset=length_type=="TL",col="red")
	points(log(K)~log(Linf),dat,subset=length_type=="SL",col="green")
	points(log(K)~log(Linf),dat,subset=length_type=="FL",col="blue")
	}
	
if(n==2)
	{
	plot(exp(-K)~log(Linf),dat,type="n")
	points(exp(-K)~log(Linf),dat,subset=length_type=="NG")
	points(exp(-K)~log(Linf),dat,subset=length_type=="TL",col="red")
	points(exp(-K)~log(Linf),dat,subset=length_type=="SL",col="green")
	points(exp(-K)~log(Linf),dat,subset=length_type=="FL",col="blue")
	}
	
if(n==3)
	{
	plot(log(K)~log(Linf),dat,subset=length_type=="NG")
	}
	
if(n==4)
	{
	plot(K~Linf,dat)
	}

}