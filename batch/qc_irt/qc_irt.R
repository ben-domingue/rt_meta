##need to add rapid to all data for those that don't have a rapid rsponse flag
qc<-function(x,repeated.trials=FALSE) {
    ## We only study items with dichotomously coded responses.
    x<-x[!is.na(x$resp),]
    x<-x[x$resp %in% 0:1,]
    ## For those data with such flags, we excluded responses that were coded as being uninformative due to having occurred too rapidly.
    x<-x[!x$rapid,]
    ## Timed out responses
    ## Removal of items that were never answered correctly or incorrectly
    m<-by(x$resp,x$item,mean,na.rm=TRUE)
    nms<-names(m)[m>0 & m<1]
    x<-x[x$item %in% nms,]
    ## Multiple responses
    if (!repeated.trials) {
        id<-paste(x$id,x$item)
        tab<-table(id)
        tab<-tab[tab==1]
        x<-x[id %in% names(tab),]
    }
    ## Should we impose a max time limit? Maybe 10x the median RT or something?
    m0<-quantile(x$rt,.9,na.rm=TRUE)
    max.time<- 5*exp(m0)
    test <- x$rt<log(max.time)
    x<-x[test,]
    print(table(test))
    ## N response per item
    tab<-table(x$item)
    tab<-tab[tab>=20]
    x<-x[x$item %in% names(tab),]
    ## N responses per person
    tab<-table(x$id)
    tab<-tab[tab>=10]
    x<-x[x$id %in% names(tab),]
    ##
    x
}

irt<-function(x,lmer.flag) {
    if (lmer.flag) { ##lmer
        library(lme4)
        m<-glmer(resp~0+(1|item)+(1|id),x,family="binomial")
        #m<-lmer(resp~0+(1|item)+(1|id),x)
        ranef(m)$item->fe
        item<-data.frame(item=rownames(fe),diff=-1*fe[,1])
        re<-ranef(m)$id
        stud<-data.frame(id=rownames(re),th=re[,1])
        x<-merge(x,item)
        x<-merge(x,stud)
    } else { ##mirt
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
        ##
        th<-fscores(m)
        stud<-data.frame(id=resp$id,th=th[,1])
        ##
        x<-merge(x,stud)
        x<-merge(x,item)
    }
    x
}
