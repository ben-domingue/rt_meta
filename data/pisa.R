readRDS("/home/bd/Dropbox/projects/pisa/data/resp.rds")->resp
readRDS("/home/bd/Dropbox/projects/pisa/data/rt.rds")->rt
resp[,c("country","stuid","test_version","base_form")]->x1
rt[,c("country","stuid","test_version","base_form")]->x2
L<-list()
for (i in 4:185) {
    tmp<-data.frame(x1,resp=resp[,i],rt=rt[,i],item=names(resp)[i])
    L[[i]]<-tmp[!is.na(tmp$resp),]
} 
do.call("rbind",L)->df
data.frame(df)->df

test1<-grepl("United States",df$country)
test2<-grepl("USA",df$country)
df<-df[test1 | test2,]

L<-split(df,df$item)
for (i in 1:length(L)) L[[i]]<-L[[i]][,c("stuid","resp")]
resp<-L[[1]]
names(resp)[2]<-names(L)[1]
for (i in 2:length(L)) {
    tmp<-L[[i]]
    names(tmp)[2]<-names(L)[i]
    resp<-merge(resp,tmp,all=TRUE)
}


## id<-unique(x$id)
## id<-sample(id,10000)
## x<-x[x$id %in% id,]


library(mirt)
m<-mirt(resp[,-1],1,"Rasch")
th<-fscores(m)
stud<-data.frame(stuid=resp$stuid,th=th[,1])
co<-coef(m)
co<-do.call("rbind",co[-length(co)])
item<-data.frame(item=names(resp)[-1],diff=-1*co[,2])

df<-merge(df,stud)
df<-merge(df,item)

x<-df
rm("df")

names(x)->nms
index<-grep("stuid",nms)
names(x)[index]<-"id"

x$th-x$diff -> del
exp(del)->k
k/(1+k)->x$pv

x$rt<-log(x$rt/1000)

save(x,file="/home/bd/Dropbox/projects/rt_meta/data/pisa_long_subsample.Rdata")

