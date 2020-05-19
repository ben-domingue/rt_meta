##sim data
library(LNIRT)
z<-simLNIRT(10000, 50, rho= 0.5)

##estimate irt models
library(mirt)
m<-mirt(as.data.frame(z$Y),itemtype="Rasch",1)
th<-fscores(m)
co<-coef(m)
co<-co[-length(co)]
co<-do.call("rbind",co)[,2]

##create long data
rt<-z$RT
resp<-z$Y
id<-1:nrow(rt)
item<-1:ncol(rt)
L<-list()
for (i in 1:ncol(rt)) {
    L[[i]]<-data.frame(resp=resp[,i],rt=rt[,i],id=id,item=item[i],th=th[,1],diff=-1*co[i])
}
x<-data.frame(do.call("rbind",L))

x$th-x$diff -> del 
exp(del)->k
k/(1+k)->x$pv

save(x,file="vdl_long_sim.Rdata")

##if devtools works for you
##library(devtools)
##source_url("https://www.dropbox.com/s/37w9ygroi8c8oiz/interplay_fun.R")

##otherwise
system("wget https://www.dropbox.com/s/37w9ygroi8c8oiz/interplay_fun.R")
source("interplay_fun.R")

interplay(x)
