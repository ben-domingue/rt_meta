
twopl<-function(x) {
    ##muck with item names
    nms<-unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
    ##make response matrix
    id<-unique(x$id)
    L<-split(x,x$item)
    out<-list()
    for (i in 1:length(L)) {
        z<-L[[i]]
        index<-match(z$id,id)
        resp<-rep(NA,length(id))
        resp[index]<-z$resp
        out[[i]]<-resp
    }
    resp<-do.call("cbind",out)
    resp<-data.frame(resp)
    names(resp)<-names(L)
    resp$id<-id
    nr<-apply(resp,2,function(x) length(table(x)))
    resp<-resp[,nr>1]
    ##
    library(mirt)
    index<-grep('id',names(resp))
    ni<-ncol(resp)-1
    s<-paste("F=1-",ni,"
             PRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2)",sep="") #-1.5
    model<-mirt.model(s)
    m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000))
    ##m<-mirt(resp[,-index],1,"Rasch")
    co<-coef(m)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=names(resp)[-index],diff=-1*co[,2],load=co[,1])
    ##
    th<-fscores(m)
    stud<-data.frame(id=resp$id,th=th[,1])
    ##
    x<-merge(x,stud)
    x<-merge(x,item)
    ##
    kk<-x$load*x$th-x$diff
    kk<-exp(kk)
    x$pv.2pl<-kk/(1+kk)
    ##
    x
}

##############################################################################
##IRT
setwd("/home/bd/Dropbox/projects/rt_meta/data/2_preirt")
lf<-list.files()

lmer.flag<-list(
    raw_rr98_accuracy.Rdata=TRUE,
    raw_hf_long_m1t.Rdata=TRUE,
    raw_hf_long_m2t.Rdata=TRUE,
    raw_hf_long_ft.Rdata=TRUE,
    raw_assistments.Rdata=TRUE,
    raw_msit.Rdata=TRUE,
    raw_working_memory.Rdata=TRUE,
    raw_race.Rdata=TRUE,
    raw_motion.Rdata=TRUE,
    ##adding the below as 2pl is not working for them
    raw_ldt.Rdata=FALSE
)


for (fn in lf) {
    print(fn)
    load(fn)
    test<- fn %in% names(lmer.flag)
    if (!test) {
        x<-twopl(x)
        fn2<-gsub("^raw_","",fn)
        save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/misc/2pl/2pl",fn2,sep='')) #local
    }
}





