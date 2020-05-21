desc1<-function(x) {
    ll<-function(x,p='pv') {
        z<-log(x[[p]])*x$resp+log(1-x[[p]])*(1-x$resp)
        z<-sum(z)/nrow(x)
        exp(z)
    }    
    np<-length(unique(x$id))
    ni<-length(unique(x$item))
    nn<-nrow(x)
    ##
    m<-by(x$resp,x$item,mean,na.rm=TRUE)
    tmp<-data.frame(item=names(m),itemp=as.numeric(m))
    x<-merge(x,tmp)
    x<-x[x$itemp>0 & x$itemp<1,]
    itemp<-ll(x,p='itemp')
    ##
    p<-ll(x)
    c(np,ni,nn,itemp,p)
}

desc2<-function(x) {
    den<-density(x$rt,na.rm=TRUE)
    den<-data.frame(x=den$x,y=den$y)
    ##
    ma<-by(x$resp,x$item,mean,na.rm=TRUE)
    mt<-by(x$rt,x$item,mean,na.rm=TRUE)
    w<-by(x$item,x$item,length)    
    ma<-weighted.mean(ma,w)
    mt<-weighted.mean(mt,w)
    ##
    #np<-length(unique(x$id))
    #ni<-length(unique(x$item))
    list(den=den,vals=c(ma,mt))
}
