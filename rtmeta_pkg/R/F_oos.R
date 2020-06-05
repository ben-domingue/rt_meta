
oos_pred<-function(x,pv.lmer=TRUE) {
    lll<-rms<-list()
    ll<-function(x,p='pv') {
        z<-log(x[[p]])*x$resp+log(1-x[[p]])*(1-x$resp)
        z<-sum(z)/nrow(x)
        exp(z)
    }    
    rmse<-function(x,p='pv') {
        z<-(x$resp-x[[p]])^2#log(x[[p]])*x$resp+log(1-x[[p]])*(1-x$resp)
        sqrt(mean(z))
    }    
    ##
    id<-paste(x$id,x$item)
    tab<-table(id)
    #
    if (!pv.lmer) x<-x[id %in% names(tab)[tab==1],] #mirt approach won't allow for missing
    ##standardize time
    x<-x[!is.na(x$rt),]
    m<-by(x$rt,x$item,mean,na.rm=TRUE)
    s<-by(x$rt,x$item,sd,na.rm=TRUE)
    tmp<-data.frame(item=names(m),m=as.numeric(m),s=as.numeric(s))
    x<-merge(x,tmp)
    x$rt<-(x$rt-x$m)/x$s
    NULL->x$m->x$s
    ##in & out
    n<-round(.1*nrow(x))
    range.test<-TRUE ##want to ensure that the RT for the in-sample bit covers the full range for the oos bit
    while (range.test) {
        in.out<-sort(sample(1:nrow(x),n))
        in.in<-1:nrow(x)
        in.in<-in.in[-in.out]
        oos<-x[in.out,]
        tmp<-x[in.in,]
        osr<-range(oos$rt,na.rm=TRUE)
        isr<-range(tmp$rt,na.rm=TRUE)
        range.test<- osr[1]<isr[1] | osr[2]>isr[2]
    }
    x<-tmp
    ##mirt or lmer needs to create
    ##lmer
    if (pv.lmer) {
        library(lme4)
        m<-glmer(resp~0+(1|item)+(1|id),x,family="binomial")
        #m<-lmer(resp~0+(1|item)+(1|id),x)
        ranef(m)$item->fe
        item<-data.frame(item=rownames(fe),diff=-1*fe[,1])
        re<-ranef(m)$id
        stud<-data.frame(id=rownames(re),th=re[,1])
    }
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
    cm<-colMeans(resp,na.rm=TRUE)
    itemp<-data.frame(item=names(L),itemp=cm)
    resp$id<-id
    nr<-apply(resp,2,function(x) length(table(x)))
    resp<-resp[,nr>1]
    if (!pv.lmer) {
        ##mirt
        ##get item difficulties
        newdiff<-!grepl("nwea",fn)
        #newdiff<-newdiff & !grepl("assistments",fn)
        #newdiff<-FALSE
        if (newdiff) {
            rs<-rowSums(!is.na(resp))
            resp<-resp[rs>1,]
            library(mirt)
            index<-grep('id',names(resp))
            m<-mirt(resp[,-index],1,"Rasch")
            co<-coef(m)
            co<-do.call("rbind",co[-length(co)])
            item<-data.frame(item=names(resp)[-index],diff=-1*co[,2])
        } else {
            co<-x[,c("item","diff")]
            item<-co[!duplicated(co$item),]
            item$diff<- (item$diff-mean(item$diff,na.rm=TRUE))/sd(item$diff,na.rm=TRUE) #given that the nwea parameters are on weird scale
        }
        ##get theta
        x$th<-x$diff<-x$pv<-NULL
        x<-merge(x,item)
        get.th<-function(x) {
            if (all(0:1 %in% x$resp)) {
                sigmoid<-function(x) 1/(1+exp(-x))
                ll<-function(th,x) {
                    ##
                    p<- (th - x$diff)
                    p<-sigmoid(p)
                    loglik<-x$resp*log(p) + (1-x$resp)*log(1-p)
                    sum(loglik)
                }
                ##optimization
                fit<-optim(0,ll,
                           x=x,control=list("fnscale"=-1),
                           lower=-5,
                           upper=5,
                           method="Brent",
                           hessian=TRUE
                           )
                fit$par
            } else NA
        }
        library(parallel)
        L2<-split(x,x$id)
        L2<-mclapply(L2,get.th,mc.cores=25)
        stud<-data.frame(id=names(L2),th=unlist(L2))
    }
    ##now begin work with oos
    NULL->oos$th->oos$diff->oos$pv
    df<-merge(oos,stud)
    df<-merge(df,item)
    ##
    df$th-df$diff -> del
    exp(del)->k
    df$pv<-k/(1+k)
    ##
    df<-merge(df,itemp)
    df<-df[!is.na(df$resp) & !is.na(df$pv),]
    df<-df[df$itemp>0 & df$itemp<1,]
    mean(df$resp,na.rm=TRUE)->df$p000
    ##baseline
    lll$base<-ll(df,p='p000')
    rms$base<-rmse(df,p='p000')
    lll$item<-ll(df,p='itemp')
    rms$item<-rmse(df,p='itemp')
    lll$irt<-ll(df,p='pv')
    rms$irt<-rmse(df,p='pv')
    ##
    NULL->x$th->x$diff->x$pv
    x<-merge(x,stud)
    x<-merge(x,item)
    x$th-x$diff -> del
    exp(del)->k
    x$pv<-k/(1+k)
    ##splines
    library(splines)
    spl<-bs(x$rt,df=4)
    ##models--both
    for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
    m2<-glm(resp~pv+rt,x,family="binomial")
    m3<-glm(resp~pv+spl1+spl2+spl3+spl4,x,family="binomial")
    df$pv.rt2<-predict(m2,df,type='response')
    lll$rt.lin<-ll(df,p='pv.rt2')
    rms$rt.lin<-rmse(df,p='pv.rt2')
    spl<-predict(spl,df$rt)
    for (i in 1:ncol(spl)) spl[,i]->df[[paste("spl",i,sep='')]]
    df$pv.rt<-predict(m3,df,type='response')
    lll$rt2<-ll(df,p='pv.rt')
    rms$rt2<-rmse(df,p='pv.rt')
    ##models-just time
    m4<-glm(resp~rt,x,family="binomial")
    m5<-glm(resp~spl1+spl2+spl3+spl4,x,family="binomial")
    df$pv.rtonly2<-predict(m4,df,type='response')
    lll$rt.only.lin<-ll(df,p='pv.rtonly2')
    rms$rt.only.lin<-rmse(df,p='pv.rtonly2')
    df$pv.rtonly<-predict(m5,df,type='response')
    lll$rt.only<-ll(df,p='pv.rtonly')
    rms$rt.only<-rmse(df,p='pv.rtonly')
    ##
    list(lll,rms)
}




