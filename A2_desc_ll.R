
## M<-numeric()
## for (i in 1:length(filenames)) {
##     load(filenames[[i]])
##     ##
##     M[i]<-mean(x$rt,na.rm=TRUE)
## }

    
ll<-function(x,p='pv') {
    z<-log(x[[p]])*x$resp+log(1-x[[p]])*(1-x$resp)
    z<-sum(z)/nrow(x)
    exp(z)
}    
L<-list()
for (i in 1:length(filenames)) {
    print(i)
    load(filenames[[i]])
    ##
    np<-length(unique(x$id))
    ni<-length(unique(x$item))
    nn<-nrow(x)
    ##
    m<-by(x$resp,x$item,mean,na.rm=TRUE)
    tmp<-data.frame(item=names(m),itemp=as.numeric(m))
    x<-merge(x,tmp)
    x<-x[x$itemp>0 & x$itemp<1,]
    itemp<-ll(x,p='itemp')
    ##
    p<-ll(x)
    L[[i]]<-c(np,ni,nn,itemp,p)
}

names(L)<-names(filenames)
tab<-do.call("rbind",L)

library(xtable)
xtable(tab)




########################################
#########################################

ll<-function(x,p='pv') {
    z<-log(x[[p]])*x$resp+log(1-x[[p]])*(1-x$resp)
    z<-sum(z)/nrow(x)
    exp(z)
}    
ff<-function(fn) {
    load(fn)
    ##
    id<-paste(x$id,x$item)
    tab<-table(id)
    print(fn)
    print(table(tab))
    x<-x[id %in% names(tab)[tab==1],]
    ##in & out
    n<-round(.1*nrow(x))
    in.out<-sort(sample(1:nrow(x),n))
    in.in<-1:nrow(x)
    in.in<-in.in[-in.out]
    oos<-x[in.out,]
    x<-x[in.in,]
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
    ##get item difficulties
    newdiff<-!grepl("nwea",fn)
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
    ll0<-ll(df,p='itemp')
    ll1<-ll(df,p='pv')
    ##now need to add the time model, first estimating in-sample
    NULL->x$th->x$diff->x$pv
    x<-merge(x,stud)
    x<-merge(x,item)
    x$th-x$diff -> del
    exp(del)->k
    x$pv<-k/(1+k)
    m<-by(x$rt,x$item,mean,na.rm=TRUE)
    s<-by(x$rt,x$item,sd,na.rm=TRUE)
    tmp<-data.frame(item=names(m),m=as.numeric(m),s=as.numeric(s))
    x<-merge(x,tmp)
    x$rt<-(x$rt-x$m)/x$s
    m2<-glm(resp~pv+rt,x,family="binomial")
    ##
    df<-df[!is.na(df$rt),]
    df<-merge(df,tmp)
    df$rt<-(df$rt-df$m)/df$s
    df$pv.rt<-predict(m2,df,type='response')
    ll2<-ll(df,p='pv.rt')
    c(ll0,ll1,ll2)
}


setwd("~/rt_meta/")
filenames <-
list(`RR98 Accuracy` = "rr98_accuracy.Rdata", `Hearts Flowers` = "hf_long.Rdata", 
    Hierarchical = "vdl_long_sim.Rdata", DD = "DIFFirt_long_sim.Rdata", 
    Arithmetic = "abcd.Rdata", Groupitizing = "abcd_group.Rdata", 
    Rotation = "dd_rotation.Rdata", Set = "set.Rdata", `Letter Chaos` = "letterchaos.Rdata", 
    `Add Subtract` = "add.subtract.Rdata", `Mult Div` = "multiply.divide.Rdata", 
    Chess = "chess.Rdata", Assistments = "assistments.Rdata", 
    PIAAC = "Prgusap1_2017.Rdata", PISA = "pisa_sample.Rdata", 
    `NWEA Grade 3` = "nwea_longpull_Winter_3_all.Rdata", `State Test` = "stateTest.Rdata", 
    `NWEA Grade 8` = "nwea_longpull_Winter_8_all.Rdata")

tab<-list()
filenames<-filenames[-c(1,2,13)]
for (iii in 1:length(filenames)) {
    tab[[names(filenames)[iii] ]]<-ff(filenames[[iii]])
    cl<-lapply(tab,class)
    tab.pr<-do.call("rbind",tab[cl=='numeric'])
    dump("tab.pr","")
}
dump('tab','')


tab <-
list(Hierarchical = c(0.545159768469895, 0.635949501379163, 0.629852019529017
), DD = c(0.571524173011406, 0.573611247016671, 0.572965458154158
), Arithmetic = c(0.733577722260621, 0.758528432376423, 0.75480793664427
), Groupitizing = c(0.74703365320414, 0.79163186787741, 0.795030737708188
), Rotation = c(0.637682179483253, 0.662104342450539, 0.664304187951157
), Set = c(0.574869968858706, 0.562782489160411, 0.56784857965722
), `Letter Chaos` = c(0.545469408252488, 0.599445561105875, 0.596654556041672
), `Add Subtract` = c(0.568834199934641, 0.569111459005505, 0.569099974733311
), `Mult Div` = c(0.570028446422707, 0.566010064920537, 0.567592131094978
), Chess = c(0.61737215700602, 0.672909206145369, 0.674534336513434
), PIAAC = c(0.657011396956108, 0.782877574352203, 0.817966645530603
), PISA = c(0.534750337833254, 0.576245243192357, 0.575209364197949
), `NWEA Grade 3` = c(0.51119500435618, 0.511139603075451, 0.507735012607335
), `State Test` = c(0.617096429228106, 0.655367619735881, 0.656042772666115
), `NWEA Grade 8` = c(0.510854123964928, 0.512074116106046, 0.507701358861554
))
tab<-do.call("rbind",tab)

library(gplots)
par(mgp=c(2,1,0),mar=c(3,10,1,1))
barplot2(t(tab-.5),beside=TRUE,horiz=TRUE,las=2,col=c("blue","red","pink"),xlim=c(0,.5),xaxt='n')
axis(side=1,at=c(0,.25,.5),c(".5",".75","1"))
legend("topright",bty='n',rev(c("item p","irt p","irt p + rt")),fill=rev(c("blue","red","pink")))
for (v in seq(0,.5,by=.05)) abline(v=v,col='gray',lwd=.5)

