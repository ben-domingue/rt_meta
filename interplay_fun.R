interplay<-function(x,std.time.in.item=FALSE,nspl=4,plot.den=TRUE,top.plot=TRUE,lm.line=TRUE,fe.terms='item+id',bottom.plot=TRUE,xlab="time (10th to 90th percentile)",...) {
    ##x needs to have columns:
    ## item [item id]
    ## id [person id]
    ## diff [item difficulty]
    ## th [person theta]
    ## pv [irt-based p-value]
    ## rt [response time in metric you want to analyze]
    ## resp [item response]
    #####################################################################
    nms<-c("item","id","diff","th","pv","rt")
    if (!(all(nms %in% names(x)))) stop("need more columns")
    ##standardizd item times within item
    if (std.time.in.item) {
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
    #############################################################################
    ##splining p-values
    library(splines)
    ## bs(x$pv,df=nspl)->spl
    ## for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
    ## library(fixest) ##won't work on ozzy
    ## fm.spl<-paste(paste("spl",1:nspl,sep=""),collapse="+")
    ## fm<-paste("rt~1+resp*(",fm.spl,")|",fe.terms,sep="")
    ## feols(formula(fm),x)->m
    ## fm<-paste("rt~1+resp*(",fm.spl,")",sep="")
    ## lm(fm,x)->m.lm
    ## ##fit rt based on models
    ## fe<-fixef(m)
    ## M<-mean(fe$id)
    ## index<-which.min(abs(fe$id-M))
    ## id<-names(fe$id)[index]
    ## M<-mean(fe$item)
    ## index<-which.min(abs(fe$item-M))
    ## item<-names(fe$item)[index]
    ## ##fitted values
    ## resp<-0:1
    ## qu<-quantile(x$pv,c(.05,.95))
    ## xv<-seq(qu[1],qu[2],length.out=100)
    ## predict(spl,xv)->tmp
    ## for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
    ## ##
    ## z<-expand.grid(resp=resp,pv.num=1:nrow(tmp))
    ## tmp<-data.frame(pv.num=1:100,tmp)
    ## z<-merge(z,tmp)
    ## z<-merge(z,data.frame(pv.num=1:100,pv=xv))
    ## z$item<-item
    ## z$id<-id
    ## z$lrt.lm<-predict(m.lm,z)
    ## z$lrt<-predict(m,z,"response")
    ## ##plotting
    ## rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
    ## L<-split(z,z$resp)
    ## par(mgp=c(2,1,0))
    ## if (top.plot) {
    ##     plot(NULL,xlim=c(0,1),ylim=rt.lims,xlab="probability",ylab="",...)
    ##     mtext(side=2,"fitted time (10th-90th percentile\nof empirical distribution)",line=2,cex=.75)
    ##     for (i in 1:length(L)) {
    ##         lines(L[[i]]$pv,L[[i]]$lrt,lty=i,lwd=2)
    ##         lines(L[[i]]$pv,L[[i]]$lrt.lm,lty=i,col="red",lwd=2)
    ##     }
    ##     abline(h=0,col="gray")
    ##     np<-length(unique(x$id))
    ##     ni<-length(unique(x$item))
    ##     legend("topleft",bty="n",lty=1:2,c("resp=0","resp=1"),title=paste(np,"people;",ni,"items"))
    ##     if (plot.den) {
    ##         den<-density(x$pv)
    ##         scale.factor<-.25
    ##         m<-min(den$y)
    ##         dy<-den$y-m
    ##         M<-max(den$y)
    ##         dy<-dy/M
    ##         dy<-rt.lims[1]+scale.factor*dy*(rt.lims[2]-rt.lims[1])
    ##         lines(den$x,dy,col="gray")
    ##         col<-col2rgb('gray')/255
    ##         col<-rgb(col[1],col[2],col[3],alpha=.5)
    ##         polygon(c(den$x,rev(den$x)),c(rep(rt.lims[1],length(dy)),rev(dy)),col=col)
    ##         #text(.5,rt.lims[1]+(rt.lims[2]-rt.lims[1])*scale.factor*.1,"density in gray")
    ##     }
    ## }
    #############################################################################
    ##now model accuracy
    rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
    x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
    library(splines)
    bs(x$rt,df=nspl)->spl
    for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
    library(fixest) ##won't work on ozzy
    fm.spl<-paste(paste("spl",1:nspl,sep=""),collapse="+")
    fm<-paste("resp~1+pv.center+(",fm.spl,")",sep='')
    fm.fe<-paste(fm,"|",fe.terms,sep="")
    feols(formula(fm.fe),x)->m
    lm(fm,x)->m.lm
    ##fitted accuracy
    fe<-fixef(m)
    M<-mean(fe$id)
    index<-which.min(abs(fe$id-M))
    id<-names(fe$id)[index]
    M<-mean(fe$item)
    index<-which.min(abs(fe$item-M))
    item<-names(fe$item)[index]
    ##fitted values
    pv<-0
    xv<-seq(rt.lims[1],rt.lims[2],length.out=100)
    predict(spl,xv)->tmp
    for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
    ##
    z<-expand.grid(pv.center=pv,rt.num=1:nrow(tmp))
    tmp<-data.frame(rt.num=1:100,tmp)
    z<-merge(z,tmp)
    z<-merge(z,data.frame(rt.num=1:100,rt=xv))
    z$item<-item
    z$id<-id
    z$resp<-predict(m,z,"response")
    z$resp.lm<-predict(m.lm,z)
    ##plotting
    z$resp<-z$resp-mean(z$resp)
    z$resp.lm<-z$resp.lm-mean(z$resp.lm)
    if (bottom.plot) {
        par(mgp=c(2,1,0))
        if (!top.plot) {
            plot(z$rt,z$resp,xlim=rt.lims,ylim=c(-.18,.18),xlab=xlab,ylab="Offset to Pr(x=1)",type="l",lwd=3,col='red',...)
        } else {
            plot(z$rt,z$resp,xlim=rt.lims,ylim=c(-.18,.18),xlab=xlab,ylab="Offset to Pr(x=1)",type="l",lwd=3,col='red')
        }
        if (lm.line) lines(z$rt,z$resp.lm,col="gray",lwd=3)
        abline(h=0,col='gray')
        ##
        if (lm.line) legend("topleft",bty="n",fill=c("red","black"),c("no fe","item/person fe"))
        ## if (plot.den) {
        ##     xcenter<-mean(rt.lims)
        ##     c(-.2,.2)->rt.lims
        ##     den<-density(x$rt)
        ##     scale.factor<-.25
        ##     m<-min(den$y)
        ##     dy<-den$y-m
        ##     M<-max(den$y)
        ##     dy<-dy/M
        ##     dy<-rt.lims[1]+scale.factor*dy*(rt.lims[2]-rt.lims[1])
        ##     lines(den$x,dy,col="blue")
        ##     col<-col2rgb('blue')/255
        ##     col<-rgb(col[1],col[2],col[3],alpha=.5)
        ##     polygon(c(den$x,rev(den$x)),c(rep(rt.lims[1],length(dy)),rev(dy)),col=col)
        ##     #text(xcenter,rt.lims[1]+(rt.lims[2]-rt.lims[1])*scale.factor*.1,"density in blue")
        ## }
        if (plot.den) {
            xcenter<-mean(rt.lims)
            c(-.2,.2)->rt.lims
            for (resp in 0:1) {
                den<-density(x$rt[x$resp==resp])
                scale.factor<-.25
                m<-min(den$y)
                dy<-den$y-m
                M<-max(den$y)
                dy<-dy/M
                dy<-rt.lims[1]+scale.factor*dy*(rt.lims[2]-rt.lims[1])
                lines(den$x,dy,col="blue",lty=resp+1)
                #col<-col2rgb('blue')/255
                #col<-rgb(col[1],col[2],col[3],alpha=.5)
                #polygon(c(den$x,rev(den$x)),c(rep(rt.lims[1],length(dy)),rev(dy)),col=col)
                #text(xcenter,rt.lims[1]+(rt.lims[2]-rt.lims[1])*scale.factor*.1,"density in blue")
            }
        }
    }
    NULL
}

