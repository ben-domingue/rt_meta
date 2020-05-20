lf<-list.files()

L<-list()
for (fn in lf) {
    load(fn)
    x<-res_max2
    x<-x[order(x$days),]
    id<-paste(x$user_id,x$item_id)
    x<-x[!duplicated(id),]
    ##
    x<-x[,c("user_id","item_id","response_in_milliseconds","correct_answered","days")]
    names(x)<-c("id","item","rt","resp","days")
    x$id<-paste(x$id,round(x$days/10))
    #x$id<-paste(x$id,x$days)
    tab<-table(x$id)
    x<-x[x$id %in% names(tab)[tab>9],]
    #print(table(x$resp))
    #print(table(table(x$item)))
    #print(table(x$id))
    x$rt<-log(x$rt/1000)
    L[[sub(".RData","",fn,fixed=TRUE)]]<-x
}
bigL<-L
rm("L")

for (ii in 1:length(bigL)) {
    print(ii)
    x<-bigL[[ii]]
    library(mirt)
    L<-split(x,x$item)
    for (i in 1:length(L)) L[[i]]<-L[[i]][,c("id","resp")]
    resp<-L[[1]]
    names(resp)[2]<-names(L)[1]
    for (i in 2:length(L)) {
        tmp<-L[[i]]
        names(tmp)[2]<-names(L)[i]
        resp<-merge(resp,tmp,all=TRUE)
    }
    library(mirt)
    m<-mirt(resp[,-1],1,"Rasch")
    th<-fscores(m)
    stud<-data.frame(id=resp$id,th=th[,1])
    co<-coef(m)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=names(resp)[-1],diff=-1*co[,2])
    x<-merge(x,stud)
    x<-merge(x,item)
    x$th-x$diff -> del
    exp(del)->k
    k/(1+k)->x$pv
    x->bigL[[ii]]
}


data.frame(rbind(bigL$addition,bigL$subtraction))->bigL[['add.subtract']]
data.frame(rbind(bigL$multiplication,bigL$division))->bigL[['multiply.divide']]

for (ii in 1:length(bigL)) {
    x<-bigL[[ii]]
    save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/",names(bigL)[ii],".Rdata",sep=''))
}


    ## library(lme4)
    ## m<-lmer(resp~0+factor(item)+(1|id),x)#,family="binomial")
    ## ##
    ## fe<--1*fixef(m)
    ## fe<-data.frame(item=gsub("factor(item)","",names(fe),fixed=TRUE),diff=-1*fe)
    ## x<-merge(x,fe)
    ## re<-ranef(m)$id
    ## re<-data.frame(id=rownames(re),th=re[,1])
    ## x<-merge(x,re)
    ## ##
    ## x$th-x$diff -> del
    ## exp(del)->k
    ## k/(1+k)->x$pv
