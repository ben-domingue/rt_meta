person_analysis<-function(x) {
    id<-unique(x$id)
    if (length(id)>100000) {
        id<-sample(id,100000)
        x<-x[x$id %in% id,]
    }
    ##
    library(lme4)
    mm<-by(x$rt,x$item,mean,na.rm=TRUE)
    tmp<-data.frame(item=names(mm),m=as.numeric(mm))
    x<-merge(x,tmp)
    x$rt.centered<-x$rt-x$m
    m<-lmer(rt.centered~(1|id),x)
    re<-ranef(m)$id
    re<-data.frame(id=rownames(re),tau=-1*re[,1])
    x2<-x[!duplicated(x$id),]
    x2<-merge(x2,re)
    #x2<-x2[!duplicated(x2$id),]
    coors<-cor(x2$th,x2$tau,use='p')
    ##
    L<-split(x,x$item)
    nn<-sapply(L,nrow)
    L<-L[nn>99]
    ff<-function(y) {
        ec<-ecdf(y$rt)
        y$rank<-ec(y$rt)
        y
    }
    L<-lapply(L,ff)
    x<-data.frame(do.call("rbind",L))
    y<-x[,c("id","th","rank")]
    L<-split(y,y$id)
    f<-function(x) {
        z1<-mean(x$th,na.rm=TRUE)
        z2<-sd(x$rank,na.rm=TRUE)
        c(z1,z2)
    }
    tmp<-lapply(L,f)
    tmp<-do.call("rbind",tmp)
    m<-mean(tmp[,2],na.rm=TRUE)
    r<-cor.test(tmp[,1],tmp[,2],use='p')
    c(coors,m,r$est,r$conf.int)
}

