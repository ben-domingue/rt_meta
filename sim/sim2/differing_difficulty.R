
sim<-function(N=1000,rho=0.5,nitem=25,item.diff=0,b1=1,sd.load=0) {
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
    ##
    kern<-x$load*(x$th-x$diff)+b1*x$delta
    kern<-exp(kern)
    x$pr<-kern/(kern+1)
    x$resp<-rbinom(nrow(x),1,x$pr)
    x$rt<-rnorm(nrow(x),-1*x$tau+x$delta,1)
    x<-x[,c("id","item","resp","rt")]
    library(rtmeta)
    x<-irt(x)
    x$pv.center<-x$pv-mean(x$pv)
    ##
    library(fixest)
    fm <- paste("resp~1+pv.center+rt|id+item", sep = "")
    m <- feols(formula(fm), x)
    c(rho=rho,diff=item.diff,b1=b1,sd.load=sd.load,pval=mean(x$resp),est=coef(m)[2])
}
sim()

rho<-c(-.3,0,.3)
item.diff<-seq(-3,3,length.out=5)
sd.load<-c(0,.5)

conditions<-expand.grid(item.diff=item.diff,#b1=b1,
                        rho=rho,sd.load=sd.load)
conditions<-split(conditions,1:nrow(conditions))

tab<-list()
ntimes<-5
for (i in 1:length(conditions)) {
    cons<-conditions[[i]]
    for (j in 1:ntimes)  tab[[paste(i,j)]]<-sim(rho=cons$rho,item.diff=cons$item.diff,sd.load=cons$sd.load,
                                                b1=runif(1,min=-1,max=1)
                                                )
    print(i/length(conditions))
}
tab0<-data.frame(do.call("rbind",tab))


pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/SI/differingdiff.pdf",width=6,height=6)
par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(.75,4))
pf<-function(tab) {
    diffs<-sort(unique(tab$diff))
    cols<-colorRampPalette(c("blue", "red"))(length(diffs))
    col.index<-match(tab$diff,diffs)
    plot(tab$b1,tab$est.rt,pch=19,col=cols[col.index],cex=1,
         xlab=expression(b[1]),ylab=expression(c[t]),
         xlim=c(-1,1),ylim=ran
         )
    txt<-bquote(rho~"="~.(unique(tab$rho)))
    mtext(side=3,line=0,txt)
    if (unique(tab$rho)<0) {
        legend("topleft",bty='n',fill=cols,as.character(round(diffs,2)),title=expression(bar(delta)))
        txt<-bquote("SD("~alpha~")="~.(as.character(unique(tab$sd.load))))
        legend("bottomright",bty='n',fill=NULL,legend=txt,border=NA)
    }
}
ran<-range(tab0$est.rt)
LL<-split(tab0,tab0$sd.load)
for (iii in 1:length(LL)) {
    tab<-LL[[iii]]
    L<-split(tab,tab$rho)
    lapply(L,pf)
}
dev.off()
