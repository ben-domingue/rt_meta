
## M<-numeric()
## for (i in 1:length(filenames)) {
##     load(filenames[[i]])
##     ##
##     M[i]<-mean(x$rt,na.rm=TRUE)
## }

    
ll<-function(x,p='pv') {
    z<-log(x[[p]])*x$resp+log(1-x[[p]])*(1-x$resp)
    z<-sum(z)/nrow(x)
    exp(z)
}    
L<-list()
for (i in 1:length(filenames)) {
    print(i)
    load(filenames[[i]])
    ##
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
    L[[i]]<-c(np,ni,nn,itemp,p)
}

names(L)<-names(filenames)
tab<-do.call("rbind",L)

library(xtable)
xtable(tab)
