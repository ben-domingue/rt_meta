
sim<-function(N=1000,rho=0.5,nitem=25,item.diff=0,b1=1,sd.load=0,shift,nspl=4) {
    library(MASS)
    z<-mvrnorm(N,c(0,0),matrix(c(1,rho,rho,1),2,2))
    th<-z[,1]
    tau<-z[,2]
    diff<-rnorm(nitem,mean=item.diff,sd=.5)
    ##2pl
    load<-exp(rnorm(nitem,mean=0,sd=sd.load))
    ##
    L<-list()
    for (i in 1:length(diff)) L[[i]]<-cbind(1:length(th),i,th,tau,diff[i],load=load[i])
    x<-data.frame(do.call("rbind",L))
    names(x)<-c("id","item","th","tau","diff","load")
    ##
    x$delta<-rnorm(nrow(x),mean=0,sd=1)
    x$delta2<-shift(x$delta)
    ##
    kern<-x$load*(x$th-x$diff)+b1*x$delta2
    kern<-exp(kern)
    x$pr<-kern/(kern+1)
    x$resp<-rbinom(nrow(x),1,x$pr)
    x$rt<-rnorm(nrow(x),-1*x$tau+x$delta,1)
    x$rt<-(x$rt-mean(x$rt))/sd(x$rt)
    x<-x[,c("id","item","resp","rt")]
    library(rtmeta)
    x<-irt(x)
    x$pv.center<-x$pv-mean(x$pv)
    ##
    library(splines)
    spl <- bs(x$rt, df = nspl)
    for (i in 1:ncol(spl)) x[[paste("spl", i, sep = "")]] <- spl[,i]
    ##
    library(fixest)
    fm <- paste("resp~1+spl1+spl2+spl3+spl4+pv.center|id+item", sep = "")
    m <- feols(formula(fm), x)
    ##
    fe <- fixest::fixef(m)
    M <- mean(fe$id)
    index <- which.min(abs(fe$id - M))
    id <- names(fe$id)[index]
    M <- mean(fe$item)
    index <- which.min(abs(fe$item - M))
    item <- names(fe$item)[index]
    pv <- 0
    xv<-seq(quantile(x$rt,.02),quantile(x$rt,.98),length.out=100)
    tmp <- predict(spl, xv)
    for (i in 1:ncol(tmp)) colnames(tmp)[i] <- paste("spl",i, sep = "")
    z <- expand.grid(pv.center = pv, rt.num = 1:nrow(tmp))
    tmp <- data.frame(rt.num = 1:100, tmp)
    z <- merge(z, tmp)
    z <- merge(z, data.frame(rt.num = 1:100, rt = xv))
    z$item <- item
    z$id <- id
    z$resp <- predict(m, z, "response")
    z$resp <- z$resp - mean(z$resp)
    pts <- cbind(z$rt, z$resp)
    data.frame(rt=z$rt,resp=z$resp,shift.rt=shift(z$rt))
}


L<-list()
##
b1seq<-seq(0,1,by=.25)
rho<-0
N<-10000
##linear
l<-list()
shift<-function(x) .5*x
for (b1 in b1seq) l[[as.character(b1)]]<-sim(N=N,b1=b1,rho=rho,shift=shift)
L$linear<-l
##quadratic up
l<-list()
shift<-function(x) .4*x^2
for (b1 in b1seq) l[[as.character(b1)]]<-sim(N=N,b1=b1,rho=rho,shift=shift)
L$quad.up<-l
##quadratic down
l<-list()
shift<-function(x) -.4*x^2
for (b1 in b1seq) l[[as.character(b1)]]<-sim(N=N,b1=b1,rho=rho,shift=shift)
L$quad.down<-l
##sin
l<-list()
shift<-function(x) 1.3*sin(2*x)
for (b1 in b1seq) l[[as.character(b1)]]<-sim(N=N,b1=b1,rho=rho,shift=shift)
L$sin<-l




pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/SI/differentshapes.pdf",width=6,height=6)
nms<-c("Linear","Quadratic, Up","Quadratic, Down","Sinusoidal")
f<-function(x,...) lines(x$rt,x$resp,type='l',lwd=2,...)
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1.5,1))
cols<-colorRampPalette(c("blue", "red"))(length(b1seq))
for (i in 1:length(L)) {
    plot(NULL,xlim=c(-3,3),ylim=c(-.175,.175),xlab="RT",ylab="Offset to Pr(x=1)")
    for (j in 1:length(L[[i]])) f(L[[i]][[j]],col=cols[j])
    if (i==1) legend("topleft",bty='n',fill=cols,as.character(b1seq),title=expression(b[1]))
    mtext(side=3,line=.2,nms[i])
}
dev.off()
