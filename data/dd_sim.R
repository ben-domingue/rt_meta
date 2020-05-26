library(diffIRT)
z<-simdiff(N=10000,nit=50,model="Q",max.iter=19999,eps=1e-15)


## ##estimate irt models
## library(mirt)
## m<-mirt(as.data.frame(z$x),itemtype="Rasch",1)
## th<-fscores(m)
## co<-coef(m)
## co<-co[-length(co)]
## co<-do.call("rbind",co)[,2]

##create long data
rt<-z$rt
resp<-z$x
id<-1:nrow(rt)
item<-1:ncol(rt)
L<-list()
for (i in 1:ncol(rt)) {
    L[[i]]<-data.frame(resp=resp[,i],rt=log(rt[,i]),id=id,item=item[i])#,th=th[,1],diff=-1*co[i])
}
x<-data.frame(do.call("rbind",L))

## x$th-x$diff -> del
## exp(del)->k
## k/(1+k)->x$pv

save(x,file="~/Dropbox/projects/rt_meta/data/raw_main/raw_DIFFirt_long_sim.Rdata")

