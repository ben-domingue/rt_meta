library(diffIRT)
data(rotation)
acc<-rotation[,1:10]
rt<-rotation[,11:20]

id<-1:nrow(rt)
item<-1:ncol(rt)
L<-list()
for (i in 1:ncol(rt)) {
    L[[i]]<-data.frame(resp=acc[,i],rt=rt[,i],id=id,item=item[i])
}
x<-data.frame(do.call("rbind",L))
x$rt<-log(x$rt)
x<-x[!is.na(x$rt),]

save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_dd_rotation.Rdata")

## ##estimate irt models
## library(mirt)
## m<-mirt(as.data.frame(acc),itemtype="Rasch",1)
## th<-fscores(m)
## co<-coef(m)
## co<-co[-length(co)]
## co<-do.call("rbind",co)[,2]

## ##create long data
## rt<-rt
## resp<-acc
## id<-1:nrow(rt)
## item<-1:ncol(rt)
## L<-list()
## for (i in 1:ncol(rt)) {
##     L[[i]]<-data.frame(resp=resp[,i],rt=rt[,i],id=id,item=item[i],th=th[,1],diff=-1*co[i])
## }
## x<-data.frame(do.call("rbind",L))

## x$th-x$diff -> del
## exp(del)->k
## k/(1+k)->x$pv

## save(x,file="dd_rotation.Rdata")
