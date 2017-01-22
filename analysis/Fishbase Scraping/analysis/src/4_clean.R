

xxx<- subset(xxx,done==0)


yyy<- subset(yyy,done==0)


fn<- fn[-match("yyy.csv", fn)]
fn<- fn[-match("full_dat.csv", fn)]

dat$spp<- strsplit(fn[1],"__")[[1]][1]
dat$sppid<- strsplit(fn[1],"__")[[1]][2]
dat$sppid<- as.numeric(gsub(".csv","",dat$sppid))


# VBGF PARAMETERS
fn_vbgf<- fn_vbgf[grep("age_size", fn_vbgf)]
fn_vbgf<- fn_vbgf[-match("age_size.csv", fn_vbgf)]

