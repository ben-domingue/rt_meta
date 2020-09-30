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
x<-data.frame(do.call("rbind",L))

##get rid of 0 and N response strings
rr<-aggregate(x$resp,list(x$id),sum)
bad<-rr[rr[,2] %in% c(0,length(unique(x$item))),]
x<-x[!(x$id %in% bad[,1]),]

irt.justitem<-function(x) {
    ##muck with item names
    nms<-unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
    ##make response matrix
    id<-unique(x$id)
    L<-split(x,x$item)
    out<-list()
    for (i in 1:length(L)) {
        z<-L[[i]]
        index<-match(z$id,id)
        resp<-rep(NA,length(id))
        resp[index]<-z$resp
            out[[i]]<-resp
    }
    resp<-do.call("cbind",out)
    resp<-data.frame(resp)
    names(resp)<-names(L)
    resp$id<-id
    nr<-apply(resp,2,function(x) length(table(x)))
    resp<-resp[,nr>1]
    ##
    library(mirt)
    index<-grep('id',names(resp))
    m<-mirt(resp[,-index],1,"Rasch")
    co<-coef(m)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=names(resp)[-index],diff=-1*co[,2])
    th<-fscores(m)
    stud<-data.frame(id=resp$id,th=th[,1])
    ##
    x<-merge(x,stud)
    x<-merge(x,item)
}
x1<-irt.justitem(x)
x<-x1
x$th<-NULL

##jack-knife
ml<-function(y) {
    ll<-function(th,y) {
        sigmoid<-function(x) 1/(1+exp(-x))
        p<- (th - y$diff)
        p<-sigmoid(p)
        loglik<-y$resp*log(p) + (1-y$resp)*log(1-p)
        sum(loglik)
    }
    for (i in 1:nrow(y)) {
        y0<-y[-i,,drop=FALSE]
        fit<-optim(0,ll,method="Brent",lower=-5,upper=5,
                   y=y0,
                   control=list("fnscale"=-1),
                   hessian=FALSE
                   )
        y$th[i]<-fit$par
    } 
    y
}
#
x$th<-NA
L<-split(x,x$id)
L<-lapply(L,ml)
x<-do.call("rbind",L)
x<-data.frame(x)

##x needs to have columns:
## item [item id]
## id [person id]
## diff [item difficulty]
## th [person theta]
## pv [irt-based p-value]
## rt [response time in metric you want to analyze]
## resp [item response]
kk<-x$th-x$diff
kk<-exp(kk)
x$pv<-kk/(1+kk)
kk<-x1$th-x1$diff
kk<-exp(kk)
x1$pv<-kk/(1+kk)

par(mgp=c(2,1,0),mar=c(3,3,1,1),mfrow=c(1,2))
library(rtmeta)
L<-interplay(x1)#,nboot=250)
plotSAT(L,nm='norm',xl=range(L$pts[,1]))
L<-interplay(x)#,nboot=250)
plotSAT(L,nm='jk',xl=range(L$pts[,1]))
