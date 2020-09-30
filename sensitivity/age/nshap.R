## setwd("/home/bd/Dropbox/projects/rt_meta/data/0_raw/nshap/ICPSR_36873/DS0001/")
## load("36873-0001-Data.rda")
## df<-da36873.0001
## names(df)<-tolower(names(df))
## age<-df[,c("id","age")]


## load("~/Dropbox/projects/rt_meta/data/3_ready/nshap.Rdata")
## x<-merge(x,age,by='id')
## x<-x[!is.na(x$age),]
## gr<-cut(x$age,c(-Inf,70,80,90,Inf),labels=c("<70","70-80","80-90",">90"))
## L<-split(x,gr)

## library(rtmeta)
## par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1))
## for (i in 1:length(L)) {
##     z<-interplay(L[[i]])
##     plotSAT(z,nm='')
##     legend("topright",bty='n',names(L)[i])
## }
