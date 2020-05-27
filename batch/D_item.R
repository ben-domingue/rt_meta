item_analysis<-function(x) {
    item.interplay<-function(x,std.time.in.item=FALSE) {
        nms<-c("item","id","diff","th","pv","rt")
        if (!(all(nms %in% names(x)))) stop("need more columns")
        tmp<-x[,nms]
        x<-x[rowSums(is.na(tmp))==0,]
        #############################################################################
        ##now model accuracy
        x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
        m<-lm(resp~rt+pv.center,x)
        d<-unique(x$diff)
        if (length(d)==1) c(d,summary(m)$coef[2,]) else rep(NA,5)
    }
    ##
    id<-unique(x$id)
    L<-split(x,x$item)
    nn<-sapply(L,nrow)
    L<-L[nn>50]
    #if (length(L)>250) L<-L[sample(1:length(L),250)]
    out<-list()
    for (j in 1:length(L)) {
        out[[j]]<-item.interplay(L[[j]])
    }
    x<-do.call("rbind",out)
    ##
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
    #if (r$p.value<.05) r0<-round(r$estimate,2) else r0<-''
    c(n,round(100*np/n),round(100*nn/n),r$est,r$conf.int)
}
