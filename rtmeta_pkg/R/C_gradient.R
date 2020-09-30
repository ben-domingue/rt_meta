gradfield<-function(x,ran=c(-.25,.25)) {
    ################################
    pd<-function(x,nspl=4,std.time=FALSE) {
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
        x$pv.center<-x$pv #x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
        ##
        library(splines)
        bs(x$rt,df=nspl)->spl
        for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
        ##
        est<-function(x) {
            library(fixest) ##won't work on ozzy
            fm.spl<-paste(paste("spl",1:nspl,sep=""),collapse="+")
            fm<-paste("resp~1+pv.center*(",fm.spl,")|item+id",sep="")
            feols(formula(fm),x)->m
            #if (print) print(summary(m))
            ##
            fe<-fixest::fixef(m)
            M<-mean(fe$id)
            index<-which.min(abs(fe$id-M))
            id<-names(fe$id)[index]
            M<-mean(fe$item)
            index<-which.min(abs(fe$item-M))
            item<-names(fe$item)[index]
            ##fitted values
            #pv<-seq(-.3,.3,by=.03)
            pv<-seq(.4,1,by=.03)
            rt<-quantile(x$rt,c(.05,.95),na.rm=TRUE)
            xv<-seq(rt[1],rt[2],length.out=300)
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
        z<-est(x)
        z
    }
    #####################################################################
    id<-unique(x$id)
    if (length(id)>50000) {
        id<-sample(id,50000)
        x<-x[x$id %in% id,]
    }
    L<-pd(std.time=FALSE,x)
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
    #####################################################################
    plotfun<-function(x,cols) {
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
        getcol<-function(val,cols) {
            del<-abs(val-cols$pd)
            if (!all(is.na(del))) {
                index<-which.min(del)
                cols$col[index]
            } else NA
        }
        getcol<-Vectorize(getcol,"val")
        x$col<-getcol(x$pd,cols)
        data.frame(rt=x$rt,pv=x$pv.center,col=x$col)
    }
    plotfun(L,cols)    
}
