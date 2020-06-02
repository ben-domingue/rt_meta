library(foreign)
x<-read.spss("chess.sav",to.data.frame=TRUE)
L<-list()
for (i in 1:40) {
    nms<-paste(c("A","AR","B","BR"),i,sep='')
    tmp<-x[,nms]
    z1<-data.frame(id=1:nrow(tmp),resp=tmp[,1],rt=tmp[,2],item=paste('a',i))
    L[[paste(i,'a')]]<-z1
    z1<-data.frame(id=1:nrow(tmp),resp=tmp[,3],rt=tmp[,4],item=paste('b',i))
    L[[paste(i,'b')]]<-z1
}
x<-data.frame(do.call("rbind",L))

x<-x[x$rt<30,]
log(x$rt)->x$rt
save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_chess.Rdata")


## L<-split(df,df$item)
## for (i in 1:length(L)) L[[i]]<-L[[i]][,c("id","resp")]
## resp<-L[[1]]
## names(resp)[2]<-names(L)[1]
## for (i in 2:length(L)) {
##     tmp<-L[[i]]
##     names(tmp)[2]<-names(L)[i]
##     resp<-merge(resp,tmp,all=TRUE)
## }

## rs<-rowSums(is.na(resp))
## resp<-resp[rs==0,]
## library(mirt)
## m<-mirt(resp[,-1],1,"Rasch")
## th<-fscores(m)
## stud<-data.frame(id=resp$id,th=th[,1])
## co<-coef(m)
## co<-do.call("rbind",co[-length(co)])
## item<-data.frame(item=names(resp)[-1],diff=-1*co[,2])

## df<-merge(df,stud)
## df<-merge(df,item)

## x<-df
## log(x$rt)->x$rt

## x$th-x$diff -> del
## exp(del)->k
## k/(1+k)->x$pv

## save(x,file="/home/bd/Dropbox/projects/rt_meta/data/chess.Rdata")
