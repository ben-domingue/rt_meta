x<-read.table("clean.dat",header=TRUE)
x$cond->x$item
x$sub->x$id
x$rt<-log(x$rt)

x<-x[,c("id","item","resp","rt")]
save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_race.Rdata")


## library(rtmeta)
## x<-irt(x)
## L<-interplay(x)#,nboot=250)
## plotSAT(L,nm='',xl=range(L$pts[,1]))
