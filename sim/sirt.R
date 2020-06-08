##deprecated


##https://link.springer.com/article/10.1007%2Fs11336-012-9288-y
th<-rnorm(1000,0,1)
## checking marginals for t
## library(rv)
## par(mfrow=c(1,3))
## for (th in c(-1,0,1)) {
##     d<-10
##     den.t<-function(t) {#,th,d) {
##         if (th==0) rep(1/d,length(t)) else th*(exp((d-t)*th)+exp(-1*(d-t)*th))/(exp(d*th)-exp(-1*d*th))
##     }
##     library(rv)
##     z<-rvdens(n=1,den.t,range=c(.001,d-.001))
##     hist(z[[1]])
## }
simfun<-function(th,delta) {
    d<-10
    th<-th-delta
    ##use eqn 13 to generate t
    library(rv)
    den.t<-function(t) {#,th,d) {
        if (th==0) rep(1/d,length(t)) else th*(exp((d-t)*th)+exp(-1*(d-t)*th))/(exp(d*th)-exp(-1*d*th))
    }
    library(rv)
    z<-rvdens(n=1,den.t,range=c(.001,d-.001))
    t<-z[[1]][1]
    ##use eqn 20 to generate x
    k<-2*(d-t)*th
    k<-exp(k)
    p<-k/(1+k)
    x<-rbinom(1,1,p)
    c(th,delta,x,t)
}
sf<-Vectorize(simfun)
th<-rnorm(1000)
delta<-rnorm(50)
L<-list()
for (i in 1:length(delta)) {
    z<-t(sf(th,delta[i]))
    z<-data.frame(z)
    names(z)<-c("th","diff","resp","t")
    z$id<-1:nrow(z)
    z$item<-i
    L[[i]]<-z
}
x<-data.frame(do.call("rbind",L))
x<-x[x$t>0,] ##weirdness
x$rt<-log(x$t)
NULL->x$th->x$diff
x->hold

library(rtmeta)
x<-irt(x)
L<-interplay(x)#,nboot=250)
plotSAT(L,nm='',xl=range(L$pts[,1]))
