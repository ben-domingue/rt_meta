## sat_pvalue<-function(x,std.time.in.item=FALSE,nspl=4,...) {
##     ##x needs to have columns:
##     ## item [item id]
##     ## id [person id]
##     ## diff [item difficulty]
##     ## th [person theta]
##     ## pv [irt-based p-value]
##     ## rt [response time in metric you want to analyze]
##     ##resp [item response]
##     #####################################################################
##     nms<-c("item","id","diff","th","pv","rt")
##     if (!(all(nms %in% names(x)))) stop("need more columns")
##     ##standardizd item times within item
##     if (std.time.in.item) {
##         L<-split(x,x$item)
##         std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
##         for (i in 1:length(L)) {
##             L[[i]]->y
##             y$rt<-std(y$rt)
##             L[[i]]<-y
##         }
##         x<-data.frame(do.call("rbind",L))
##     }
##     tmp<-x[,nms]
##     x<-x[rowSums(is.na(tmp))==0,]
##     ##now model accuracy
##     x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
##     library(splines)
##     bs(x$rt,df=nspl)->spl
##     for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
##     library(fixest) ##won't work on ozzy
##     feols(resp~1+pv.center*(spl1+spl2+spl3+spl4)|item+id,x)->m
##     ##fitted accuracy
##     fe<-fixef(m)
##     M<-mean(fe$id)
##     index<-which.min(abs(fe$id-M))
##     id<-names(fe$id)[index]
##     M<-mean(fe$item)
##     index<-which.min(abs(fe$item-M))
##     item<-names(fe$item)[index]
##     ##fitted values
##     rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
##     xv<-seq(rt.lims[1],rt.lims[2],length.out=100)
##     predict(spl,xv)->tmp
##     for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
##     ##
##     pv<-c(-.1,.1)
##     z<-expand.grid(pv.center=pv,rt.num=1:nrow(tmp))
##     tmp<-data.frame(rt.num=1:100,tmp)
##     z<-merge(z,tmp)
##     z<-merge(z,data.frame(rt.num=1:100,rt=xv))
##     z$item<-item
##     z$id<-id
##     z$resp<-predict(m,z,"response")
##     ##plotting
##     z$resp<-z$resp-mean(z$resp)
##     par(mgp=c(2,1,0))
##     plot(NULL,xlim=rt.lims,ylim=c(-.55,.55),xlab="time (10th to 90th percentile)",ylab="Offset to Pr(x=1)",type="l",lwd=3,...)
##     L<-split(z,z$pv.center)
##     for (z in L) lines(z$rt,z$resp,lwd=3)
##     abline(h=-.1,col='gray')
##     abline(h=.1,col='gray')
##     ##
##     ##legend("topleft",bty="n",fill=c("red","black"),c("no fe","item/person fe"))
##     NULL
## }

## sat_pvalue_split<-function(x,std.time.in.item=FALSE,nspl=4,...) {
##     ##x needs to have columns:
##     ## item [item id]
##     ## id [person id]
##     ## diff [item difficulty]
##     ## th [person theta]
##     ## pv [irt-based p-value]
##     ## rt [response time in metric you want to analyze]
##     ##resp [item response]
##     #####################################################################
##     nms<-c("item","id","diff","th","pv","rt")
##     if (!(all(nms %in% names(x)))) stop("need more columns")
##     ##standardizd item times within item
##     if (std.time.in.item) {
##         L<-split(x,x$item)
##         std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
##         for (i in 1:length(L)) {
##             L[[i]]->y
##             y$rt<-std(y$rt)
##             L[[i]]<-y
##         }
##         x<-data.frame(do.call("rbind",L))
##     }
##     tmp<-x[,nms]
##     x<-x[rowSums(is.na(tmp))==0,]
##     ##now model accuracy
##     x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
##     library(splines)
##     bs(x$rt,df=nspl)->spl
##     for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
##     library(fixest) ##won't work on ozzy
##     rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
##     L<-split(x,x$pv.center>0)
##     plot(NULL,xlim=rt.lims,ylim=c(0,1),xlab="time (10th to 90th percentile)",ylab="Offset to Pr(x=1)",type="l",lwd=3,...)
##     for (x in L) {
##         feols(resp~1+pv.center*(spl1+spl2+spl3+spl4)|item+id,x)->m
##         ##fitted accuracy
##         fe<-fixef(m)
##         M<-mean(fe$id)
##         index<-which.min(abs(fe$id-M))
##         id<-names(fe$id)[index]
##         M<-mean(fe$item)
##         index<-which.min(abs(fe$item-M))
##         item<-names(fe$item)[index]
##         ##fitted values
##         xv<-seq(rt.lims[1],rt.lims[2],length.out=100)
##         predict(spl,xv)->tmp
##         for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
##         ##
##         pv<-mean(x$pv.center,na.rm=TRUE)
##         z<-expand.grid(pv.center=pv,rt.num=1:nrow(tmp))
##         tmp<-data.frame(rt.num=1:100,tmp)
##         z<-merge(z,tmp)
##         z<-merge(z,data.frame(rt.num=1:100,rt=xv))
##         z$item<-item
##         z$id<-id
##         z$resp<-predict(m,z,"response")
##         ##plotting
##         #z$resp<-z$resp-mean(z$resp)
##         par(mgp=c(2,1,0))
##         lines(z$rt,z$resp,lwd=3)
##         abline(h=pv,col='gray')
##     }
##     NULL
## }

