

out<-list()
for (i in 1:length(filenames)) {
    print(i)
    load(filenames[[i]])
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
    r<-cor(tmp[,1],tmp[,2],use='p')
    out[[i]]<-c(coors,m,r)
}

tab<-do.call("rbind",out)
rownames(tab)<-names(filenames)
library(xtable)
xtable(tab)

## item.interplay<-function(x,std.time.in.item=FALSE) {
##     ##x needs to have columns:
##     ## item [item id]
##     ## id [person id]
##     ## diff [item difficulty]
##     ## th [person theta]
##     ## pv [irt-based p-value]
##     ## rt [response time in metric you want to analyze]
##     ##resp [item response]
##     #####################################################################
##     nms<-c("item","id","diff","th","pv","rt")
##     if (!(all(nms %in% names(x)))) stop("need more columns")
##     ##standardizd item times within item
##     if (std.time.in.item) {
##         L<-split(x,x$item)
##         std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
##         for (i in 1:length(L)) {
##             L[[i]]->y
##             y$rt<-std(y$rt)
##             L[[i]]<-y
##         }
##         x<-data.frame(do.call("rbind",L))
##     }
##     tmp<-x[,nms]
##     x<-x[rowSums(is.na(tmp))==0,]
##     if (nrow(x)>5) {
##         #############################################################################
##         rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
##         #############################################################################
##         ##now model accuracy
##         x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
##         m<-lm(resp~rt+pv.center,x)
##         d<-unique(x$th)
##         if (length(d)==1) c(d,summary(m)$coef[2,1]) else rep(NA,2)
##     } else NULL
## }


## tabL<-list()
## for (i in 1:length(filenames)) {
##     load(filenames[[i]])
##     id<-unique(x$id)
##     if (length(id)>50000) {
##         id<-sample(id,50000)
##         x<-x[x$id %in% id,]
##     }
##     L<-split(x,x$id)
##     nn<-sapply(L,nrow)
##     if (length(L)>2000) L<-L[sample(1:length(L),2000)]
##     out<-list()
##     for (j in 1:length(L)) {
##         out[[j]]<-item.interplay(L[[j]])
##     }
##     tabL[[i]]<-do.call("rbind",out)
## }


## par(mfrow=c(4,3),mar=c(3,3,1,1),oma=rep(1,4))
## for (i in 1:length(tabL)) {
##     tab<-tabL[[i]]
##     den<-density(tab[,2],na.rm=TRUE)
##     plot(den,xlab='',ylab='',main='',xlim=c(-.3,.4),yaxt="n")
##     polygon(c(den$x,rev(den$x)),c(den$y,rep(0,length(den$y))),col='blue')
##     abline(v=0,col="gray")
##     mtext(side=3,line=0,names(filenames)[i])
## }

par(mfrow=c(4,3),mar=c(3,3,1,1),oma=rep(1,4))
for (i in 1:length(tabL)) {
    tab<-tabL[[i]]
    plot(tab,pch=19,col='red',xlab='theta',ylab='Marginal effect of time')#,ylim=c(-.4,.6))
    #mtext(side=3,line=0,names(filenames)[i])
    legend("topleft",bty='n',names(filenames)[i])
    legend("bottomright",bty='n',paste("r = ",format(round(cor(tab[,1],tab[,2],use='p'),2)),sep=''))
}
