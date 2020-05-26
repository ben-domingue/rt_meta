## load("~/Dropbox/projects/mcas/data/ir.Rdata")
## ir[['e 2019 10']]->df

## library(foreign)
## read.spss("~/Dropbox/projects/mcas/data/raw/MCAS_Spring_2019_EarlyReleaseV2_ela_20190619.sav",to.data.frame=TRUE)->x
## paste("e",1:31,"_Time",sep="")->nms
## x[,c("SASID",nms)]->tmp
## names(tmp)<-tolower(names(tmp))
## gsub("^e","ir",names(tmp))->names(tmp)
## merge(df,tmp)->df

## index<-grep("ir27",names(df))
## df<-df[,-index]

## index<-grep("_time",names(df))
## rt<-df[,index]
## tmp<-df[,-index]
## index<-grep("^ir",names(tmp))
## acc<-tmp[,index]

## n<-numeric()
## for (i in 1:ncol(acc)) {
##     z<-acc[,i]
##     n[i]<-length(unique(z[!is.na(z)]))
## }
## acc<-acc[,n==2]
## rt<-rt[,n==2]


## ##estimate irt models
## library(mirt)
## m<-mirt(as.data.frame(acc),itemtype="Rasch",1)
## th<-fscores(m)
## co<-coef(m)
## co<-co[-length(co)]
## co<-do.call("rbind",co)[,2]

## ##create long data
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

## x$rt<-ifelse(x$rt==0,NA,x$rt)
## log(x$rt)->x$rt


## save(x,file="stateTest.Rdata")
