
readRDS("resp.rds")->resp
readRDS("rt.rds")->rt
resp[,c("country","stuid","test_version","base_form")]->x1
rt[,c("country","stuid","test_version","base_form")]->x2
L<-list()
for (i in 4:185) {
    tmp<-data.frame(x1,resp=resp[,i],rt=rt[,i],item=names(resp)[i])
    L[[i]]<-tmp[!is.na(tmp$resp),]
} 
do.call("rbind",L)->df
data.frame(df)->df.big

ids<-sample(unique(df.big$stuid),50000)
df<-df.big[df.big$stuid %in% ids,]


## ##
## L<-split(df,df$item)
## for (i in 1:length(L)) L[[i]]<-L[[i]][,c("stuid","resp")]
## resp<-L[[1]]
## names(resp)[2]<-names(L)[1]
## for (i in 2:length(L)) {
##     tmp<-L[[i]]
##     names(tmp)[2]<-names(L)[i]
##     resp<-merge(resp,tmp,all=TRUE)
## }
## ##
## library(mirt)
## m<-mirt(resp[,-1],1,"Rasch")
## th<-fscores(m)
## stud<-data.frame(stuid=resp$stuid,th=th[,1])
## co<-coef(m)
## co<-do.call("rbind",co[-length(co)])
## item<-data.frame(item=names(resp)[-1],diff=-1*co[,2])
## ##
## df<-merge(df,stud)
## df<-merge(df,item)
## ##
x<-df
rm("df")
##
names(x)->nms
index<-grep("stuid",nms)
names(x)[index]<-"id"
##
## x$th-x$diff -> del
## exp(del)->k
## k/(1+k)->x$pv
##
x$rt<-log(x$rt/1000)
#
x<-x[,c("id","resp","rt","item")]
save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_pisa_sample.Rdata")
