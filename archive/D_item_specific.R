

item.interplay<-function(x,std.time.in.item=FALSE) {
    ##x needs to have columns:
    ## item [item id]
    ## id [person id]
    ## diff [item difficulty]
    ## th [person theta]
    ## pv [irt-based p-value]
    ## rt [response time in metric you want to analyze]
    ##resp [item response]
    #####################################################################
    nms<-c("item","id","diff","th","pv","rt")
    if (!(all(nms %in% names(x)))) stop("need more columns")
    ##standardizd item times within item
    if (std.time.in.item) {
        L<-split(x,x$item)
        std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
        for (i in 1:length(L)) {
            L[[i]]->y
            y$rt<-std(y$rt)
            L[[i]]<-y
        }
        x<-data.frame(do.call("rbind",L))
    }
    tmp<-x[,nms]
    x<-x[rowSums(is.na(tmp))==0,]
    #############################################################################
    rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
    #############################################################################
    ##now model accuracy
    x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
    m<-lm(resp~rt+pv.center,x)
    d<-unique(x$diff)
    if (length(d)==1) c(d,summary(m)$coef[2,]) else rep(NA,5)
}



tabL<-list()
for (i in 1:length(filenames)) {
    load(filenames[[i]])
    id<-unique(x$id)
    ## if (length(id)>50000) {
    ##     id<-sample(id,50000)
    ##     x<-x[x$id %in% id,]
    ## }
    L<-split(x,x$item)
    nn<-sapply(L,nrow)
    L<-L[nn>100]
    #if (length(L)>250) L<-L[sample(1:length(L),250)]
    out<-list()
    for (j in 1:length(L)) {
        out[[j]]<-item.interplay(L[[j]])
    }
    tabL[[i]]<-do.call("rbind",out)
}

f<-function(x) {
    x<-x[!is.na(x[,2]),]
    n<-nrow(x)
    alpha<-.05/n
    #library(fdrtool)
    sig<-x[x[,5]<alpha,,drop=FALSE]
    if (nrow(sig)>0) {
        np<-sum(sig[,2]>0)
        nn<-sum(sig[,2]<0)
    } else 0->np->nn
    r<-cor.test(x[,1],x[,2],use='p')
    if (r$p.value<.05) r0<-round(r$estimate,2) else r0<-''
    c(n,round(100*np/n),round(100*nn/n),r0)
}
tab<-lapply(tabL,f)
tab<-do.call("rbind",tab)
rownames(tab)<-names(filenames)
library(xtable)
xtable(tab)

## par(mfrow=c(4,4),mar=c(3,3,1,1),oma=rep(1,4))
## for (i in 1:length(tabL)) {
##     tab<-tabL[[i]]
##     den<-density(tab[,2],na.rm=TRUE)
##     plot(den,xlab='',ylab='',main='',xlim=c(-.3,.4),yaxt="n")
##     polygon(c(den$x,rev(den$x)),c(den$y,rep(0,length(den$y))),col='blue')
##     abline(v=0,col="gray")
##     mtext(side=3,line=0,names(filenames)[i])
## }

## par(mfrow=c(4,4),mar=c(3,3,1,1),oma=rep(1,4))
## for (i in 1:length(tabL)) {
##     tab<-tabL[[i]]
##     plot(tab,pch=19,col='red',ylim=c(-.4,.6))
##     mtext(side=3,line=0,names(filenames)[i])
##     legend("bottomright",bty='n',paste("r = ",format(round(cor(tab[,1],tab[,2],use='p'),2)),sep=''))
## }
