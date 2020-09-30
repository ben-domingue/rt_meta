LL<-list()
options(set.seed(88))
        
##hierarchical
library(LNIRT)
z<-simLNIRT(10000, 50, rho= 0.5)
rt<-z$RT
resp<-z$Y
id<-1:nrow(rt)
item<-1:ncol(rt)
L<-list()
for (i in 1:ncol(rt)) {
    L[[i]]<-data.frame(resp=resp[,i],rt=rt[,i],id=id,item=item[i])
}
LL$Hierarchical<-data.frame(do.call("rbind",L))

##DD
library(diffIRT)
z<-simdiff(N=10000,nit=50,model="Q",max.iter=19999,eps=1e-15)
##create long data
rt<-z$rt
resp<-z$x
id<-1:nrow(rt)
item<-1:ncol(rt)
L<-list()
for (i in 1:ncol(rt)) {
    L[[i]]<-data.frame(resp=resp[,i],rt=log(rt[,i]),id=id,item=item[i])#,th=th[,1],diff=-1*co[i])
}
LL$DD<-data.frame(do.call("rbind",L))

##speed-accuracy
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
x->LL$SARM

## ##race
## ##DOI : 10.1007/ S 11336-013-9396-3
## get.t<-function(psi,alpha,beta) { #see eqn 8
##     ##note that i will always assume the first accumulator is the correct one (eg delta=1 for that one) and thus dispense with delta
##     z<-numeric()
##     z[1]<-alpha[1]-beta+rnorm(1)
##     z[2]<-alpha[2]+rnorm(1)
##     z[3]<-alpha[3]+rnorm(1)
##     z<-exp(z)
##     index<-which.min(z)
##     t<-psi+z[index]
##     resp<-ifelse(index==1,1,0)
##     c(resp,t)
## }
## th<-rnorm(1000) #this is beta
## psi<-runif(1000,1,2)
## library(MASS)
## alpha<-mvrnorm(50,c(-.5,.8,.95),matrix(c(1,.8,.6,.8,1,.85,.6,.85,1),3,3,byrow=TRUE))
## L<-list()
## for (i in 1:length(th)) for (j in 1:nrow(alpha)) L[[paste(i,j)]]<-c(i,j,get.t(psi[i],alpha=alpha[j,],beta=th[i]))
## x<-data.frame(do.call("rbind",L))
## names(x)<-c("id","item","resp","rt")
## log(x$rt)->x$rt
## x->LL$Race

##extended hierarchical
library(MASS)
z<-mvrnorm(1000,c(0,0),matrix(c(1,.5,.5,1),2,2))
th<-z[,1]
tau<-z[,2]
diff<-rnorm(25,mean=0,sd=.5)
L<-list()
for (i in 1:length(diff)) L[[i]]<-cbind(1:length(th),i,th,tau,diff[i])
x<-data.frame(do.call("rbind",L))
names(x)<-c("id","item","th","tau","diff")
hold<-x
bpars<-list(c(0,0),c(-.5,.5),c(.5,.5))
for (b in bpars)
                {
                    b[1]->b1
                    b[2]->b2
                    hold->x
                    x$delta<-rnorm(nrow(x),mean=0,sd=1)
                    ##
                    kern<-(x$th+b1*x$delta)-x$diff
                    kern<-exp(kern)
                    x$pr<-kern/(kern+1)
                    x$resp<-rbinom(nrow(x),1,x$pr)
                    x$rt<-rnorm(nrow(x),-1*x$tau+b2*x$delta,1)
                    x->df
                    ##
                    x<-df[,c("id","item","resp","rt")]
                    ##
                    ##
                    LL[[paste("EH: b1=",b1,",b2=",b2,sep="")]]<-x
                }


library(rtmeta)
pdf("/tmp/sat_sim.pdf",width=6,height=4)
#layout(matrix(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7),nrow=2,ncol=12,byrow=TRUE))
par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:length(LL)) {
    x<-LL[[i]]
    ##get rid of 0 and N response strings
    rr<-aggregate(x$resp,list(x$id),sum)
    bad<-rr[rr[,2] %in% c(0,length(unique(x$item))),]
    x<-x[!(x$id %in% bad[,1]),]
    ##
    x<-irt(x)
    L<-interplay(x)#,nboot=250)
    plotSAT(L,nm=names(LL)[i],xl=range(L$pts[,1]))
}
dev.off()
