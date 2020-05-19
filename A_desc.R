
M<-numeric()
for (i in 1:length(filenames)) {
    load(filenames[[i]])
    M[i]<-mean(x$rt,na.rm=TRUE)
}
filenames<-filenames[order(M)]
dump("filenames","")

## cols<-c("black","black","red","red","red","red","blue","blue","blue","blue","blue","blue")
## m<-matrix((1:length(filenames)),ncol=3,byrow=TRUE)
## m<-cbind(m,length(filenames)+1,length(filenames)+1)
## layout(m)
## par(mgp=c(2,1,0),mar=c(2,1.3,1,1),oma=rep(1,4))
## for (i in 1:length(filenames)) {
##     load(filenames[[i]])
##     ##
##     den<-density(x$rt,na.rm=TRUE)
##     plot(den,xlim=c(-3,10),type='l',col=cols[i],main='',xlab='',ylab='',yaxt='n')
##     polygon(c(den$x,rev(den$x)),c(den$y,rep(0,length(den$y))),col=cols[i])
##     np<-length(unique(x$id))
##     ni<-length(unique(x$item))
##     #legend("topright",bty="n",title=as.character(i),c(paste(np,'p'),paste(ni,'i')),cex=.875)
##     legend("topright",bty="n",title=as.character(i),legend='',cex=1.5)
## }

den<-list()
for (i in 1:length(filenames)) {
    load(filenames[[i]])
    ##
    den[[i]]<-cbind(i,x$rt)
}
den<-data.frame(do.call("rbind",den))
names(den)<-c("test","rt")

out<-list()
for (i in 1:length(filenames)) {
    load(filenames[[i]])
    ##
    ma<-by(x$resp,x$item,mean,na.rm=TRUE)
    print(filenames[i])
    print(summary(x$rt))
    mt<-by(x$rt,x$item,mean,na.rm=TRUE)
    w<-by(x$item,x$item,length)    
    ma<-weighted.mean(ma,w)
    mt<-weighted.mean(mt,w)
    ##
    #np<-length(unique(x$id))
    #ni<-length(unique(x$item))
    out[[i]]<-c(ma,mt)
}
tab<-do.call("rbind",out)


#############################################
#cols<-c("purple","purple","red","red","red","red","blue","blue","blue","blue","blue","blue")
N<-length(filenames)
cols<-rep("blue",length(filenames))
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(2,3,1,1),oma=rep(1,4))
plot(NULL,ylim=c(-9,11),xlim=c(.5,N+.5),xaxt="n",xlab='',ylab='log(t)',bty='n',xaxt='n')
axis(side=1,at=1:N,as.character(1:N),cex.axis=.7,gap.axis=0)
boxplot(rt~test,den,pch=19,col=cols,ylab="log(t)",outcex=.5,outcol=cols,add=TRUE,xaxt='n')
par(mar=c(3,3,1,7))
plot(tab[,1:2],xlab="Mean item-level accuracy",ylab="Mean item-level log(t)",cex=0,xlim=c(0,1),ylim=c(-1,5),bty="n")
text(tab[,1],tab[,2],1:nrow(tab),col=cols)
places<-seq(min(tab[,2]),max(tab[,2]),length.out=nrow(tab))
mtext(side=4,las=2,at=places,rev(paste(1:nrow(tab),names(filenames))),cex=.7,col='black',line=.2)
abline(h=log(1),col='black')
text(.2,log(1),'1 second',col="black",pos=3,cex=1.1)
abline(h=log(10),col="black")
text(.2,log(10),"10 seconds",col="black",pos=3,cex=1.1)
abline(h=log(60),col="black")
text(.2,log(60),"60 seconds",col="black",pos=3,cex=1.1)

