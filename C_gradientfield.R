pd<-function(x,nspl=4,std.time=FALSE) {
    #####################################################################
    nms<-c("item","id","diff","th","pv","rt")
    if (!(all(nms %in% names(x)))) stop("need more columns")
    ##standardizd item times within item
    if (std.time) {
        L<-split(x,x$item)
        std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
        for (i in 1:length(L)) {
            L[[i]]->y
            y$rt<-std(y$rt)
            L[[i]]<-y
        }
        x<-data.frame(do.call("rbind",L))
    }
    tmp<-x[,nms]
    x<-x[rowSums(is.na(tmp))==0,]
    #####
    x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
    ##
    library(splines)
    bs(x$rt,df=nspl)->spl
    for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
    ##
    est<-function(x,print=FALSE) {
        library(fixest) ##won't work on ozzy
        fm.spl<-paste(paste("spl",1:nspl,sep=""),collapse="+")
        fm<-paste("resp~1+pv.center*(",fm.spl,")|item+id",sep="")
        feols(formula(fm),x)->m
        if (print) print(summary(m))
        ##
        fe<-fixef(m)
        M<-mean(fe$id)
        index<-which.min(abs(fe$id-M))
        id<-names(fe$id)[index]
        M<-mean(fe$item)
        index<-which.min(abs(fe$item-M))
        item<-names(fe$item)[index]
        ##fitted values
        pv<-seq(-.3,.3,by=.01)
        rt<-quantile(x$rt,c(.05,.95),na.rm=TRUE)
        xv<-seq(rt[1],rt[2],length.out=500)
        predict(spl,xv)->tmp
        for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
        ##
        z<-expand.grid(pv.center=pv,rt.num=1:nrow(tmp))
        tmp<-data.frame(rt.num=1:nrow(tmp),tmp)
        z<-merge(z,tmp)
        z<-merge(z,data.frame(rt.num=1:nrow(tmp),rt=xv))
        z$item<-item
        z$id<-id
        z$resp<-predict(m,z,"response")
        z
    }
    z<-est(x,print=TRUE)
    z
}





L<-list()
for (i in 1:length(filenames)) {
    load(filenames[[i]])
    ##
    id<-unique(x$id)
    if (length(id)>50000) {
        id<-sample(id,50000)
        x<-x[x$id %in% id,]
    }
    L[[names(filenames)[i] ]]<-pd(std.time=FALSE,x)
}

plotfun<-function(x,main,cols) {
    ##
    ##partial derivative
    L<-split(x,x$pv.center)
    pd<-function(x) {
        x<-x[order(x$rt),]
        dy<-diff(x$resp)
        dt<-diff(x$rt)
        pd<-dy/dt
        x$pd<-c(NA,pd)
        x
    }
    L2<-lapply(L,pd)
    x<-data.frame(do.call("rbind",L2))
    ##colors
    range(x$pd,na.rm=TRUE)->ran
    print(main)
    print(ran)
    getcol<-function(val,cols) {
        del<-abs(val-cols$pd)
        if (!all(is.na(del))) {
            index<-which.min(del)
            cols$col[index]
        } else NA
    }
    getcol<-Vectorize(getcol,"val")
    x$col<-getcol(x$pd,cols)
    plot(x$rt,x$pv.center,cex=.75,col=x$col,pch=19,xlab='',ylab='')
    mtext(side=3,line=0,main)
}


ran<-c(-.25,.25)
cols<-seq(-6,6,length.out=5000)
##translate cols via the logistic
pr<-1/(1+exp(-cols))
pd<-pr*(ran[2]-ran[1])+ran[1]
cols1<-colorRampPalette(c("red", "white"))(1000)
cols2<-rev(colorRampPalette(c("blue", "white"))(1000))
cols<-c(cols1,cols2)
pv<-seq(0,1,length.out=length(cols))
col.out<-rep(NA,length(pr))
for (i in 1:length(pr)) {    
    index<-which.min(abs(pr[i]-pv))
    col.out[i]<-cols[index]
}
cols<-data.frame(pd=pd,col=col.out)

#######################################################
##plotting
##when you want a full row at the bottom for the legend
nn<-length(L)
m<-matrix(1:nn,byrow=TRUE,ncol=4)
tmp<-list()
for (i in 1:nrow(m)) rbind(m[i,],m[i,])->tmp[[i]]
m<-do.call("rbind",tmp)
m<-rbind(m,rep(1+nn,3))
##something else
m<-matrix(c(1:nn,nn+1,nn+1),nrow=4,ncol=4,byrow=TRUE)

layout(m)
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.7,4))
for (i in 1:length(L)) {
    plotfun(L[[i]],main=names(L)[i],cols=cols)
    if (i==13) {
        mtext(side=2,line=2,"Pr(x=1)")
        mtext(side=1,line=2,"log(t)")
    }
}

##color legend
par(mar=c(2,5,.2,5))
plot(cols$pd,rep(0,nrow(cols)),col=cols$col,pch=19,cex=.5,xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
mtext(side=2,#cols$pd[1],0,
      las=2,format(round(cols$pd[1],2),digits=2),col=cols$col[1])
n<-nrow(cols)
mtext(side=4,#cols$pd[n],0,
      las=2,paste(format(round(cols$pd[n],2),digits=2),"+",sep=""),col=cols$col[n])
mtext(side=1,expression(frac(partialdiff*f,partialdiff*t)),cex=.9,line=-1)

