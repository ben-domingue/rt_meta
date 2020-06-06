LL<-list()

##hierarchical
library(LNIRT)
z<-simLNIRT(10000, 50, rho= 0.5)

rt<-z$RT
resp<-z$Y
id<-1:nrow(rt)
item<-1:ncol(rt)
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
for (i in 1:ncol(rt)) {
    L[[i]]<-data.frame(resp=resp[,i],rt=log(rt[,i]),id=id,item=item[i])#,th=th[,1],diff=-1*co[i])
}
LL$DD<-data.frame(do.call("rbind",L))

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
                    LL[[paste("EH:b1=",b1,",b2=",b2,sep="")]]<-x
                }


pdf("/tmp/sat_sim.pdf",width=6,height=4)
par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:length(LL)) {
                    library(rtmeta)
                    x<-irt(LL[[i]])
                    L<-interplay(x)#,nboot=250)
                    plotSAT(L,nm=names(LL)[i],xl=range(L$pts[,1]))
}
dev.off()
