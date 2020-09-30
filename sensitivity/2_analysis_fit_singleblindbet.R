ff<-function(fn) {
    load(paste("./4_proc/proc_",fn,sep=''))
    output$oos
}
out<-lapply(filenames,ff)

tab<-lapply(out,"[[",1)
tab<-lapply(tab,unlist)
tab<-do.call("rbind",tab)
tab<-data.frame(tab)

NULL->tab$base->tab$corr

getcoins<-function(a) {
    f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
    nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
}
getcoins<-Vectorize(getcoins)
for (i in 1:ncol(tab)) getcoins(tab[,i])->tab[,i]

ew<-function(p1,p0) (p1-p0)/p0
tab->newtab
for (i in 1:nrow(tab)) for (j in 2:ncol(tab)) ew(p1=tab[i,j],p0=tab[i,1])->newtab[i,j]

par(mgp=c(2,1,0),mar=c(3,3,1,10),oma=rep(.5,4))
al<-c(-.265,.1)
plot(newtab[,c(2,4)],type='n',bty='n',ylim=al,xlim=al,xlab="E(W) based on response accuracy (C in F4)",ylab="E(W) based on accuracy + time (F in F4)")
cols<-ifelse(newtab[,2]<0,'red','black')
text(newtab[,2],newtab[,4],1:nrow(newtab),cex=.7,col=cols)
abline(v=0,col='gray')
abline(h=0,col='gray')
abline(0,1,col='gray',lty=2)
index<-seq(al[1],al[2],length.out=nrow(newtab))
nms<-paste(1:nrow(newtab),rownames(newtab))
for (i in 1:nrow(newtab)) mtext(side=4,nms[i],las=2,at=index[i],col=cols[i])
