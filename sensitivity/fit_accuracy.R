oos_acc<-function(x) { #this uses person-level predictors to predict out-of-sample responses
    lll<-list()
    acc<-function(x,p='pv') {
        x$pv<-x[[p]]
        x$z<-ifelse(x$pv>.5,1,0)
        mean(x$z==x$resp,na.rm=TRUE)
    }    
    ##standardize time
    x<-x[!is.na(x$rt) & !is.na(x$resp),]
    m<-by(x$rt,x$item,mean,na.rm=TRUE)
    s<-by(x$rt,x$item,sd,na.rm=TRUE)
    tmp<-data.frame(item=names(m),m=as.numeric(m),s=as.numeric(s))
    x<-merge(x,tmp)
    x$rt<-(x$rt-x$m)/x$s
    NULL->x$m->x$s
    ##remove respondents that got all 0s or 1s
    tmp<-aggregate(x$resp,list(x$id),mean,na.rm=TRUE)
    ids<-tmp[tmp[,2]>0 & tmp[,2]<1,1]
    x<-x[x$id %in% ids,]
    ##in & out
    n<-round(.1*nrow(x))
    in.out<-sort(sample(1:nrow(x),n))
    in.in<-1:nrow(x)
    in.in<-in.in[-in.out]
    oos<-x[in.out,]
    x<-x[in.in,]
    ##itemp
    itemp<-aggregate(x$resp,list(x$item),mean,na.rm=TRUE)
    names(itemp)<-c("item","itemp")
    itemp<-itemp[itemp$itemp>0 & itemp$itemp<1,]
    x<-x[x$item %in% itemp$item,]
    x<-merge(x,itemp)
    oos<-oos[oos$item %in% itemp$item,]
    ##baseline
    mean(x$resp,na.rm=TRUE)->oos$p000
    lll$base<-acc(oos,p='p000')
    ##item
    oos<-merge(oos,itemp)
    lll$item<-acc(oos,p='itemp')
    #response mean as a predictor
    stud<-aggregate(x$resp,list(x$id),mean)
    names(stud)<-c("id","resp.mean")
    tmp<-merge(x,stud)
    #m<-glm(resp~factor(item)+ resp.mean,tmp,family="binomial")
    m<-glm(resp~itemp+ resp.mean,tmp,family="binomial")
    oos<-merge(oos,stud)
    oos$resp.mean<-predict(m,oos,type='response')
    #response time as a predictor
    stud<-aggregate(x$rt,list(x$id),mean)
    names(stud)<-c("id","rt.mean")
    tmp<-merge(x,stud)
    #m<-glm(resp~factor(item)+ rt.mean,tmp,family="binomial")
    m<-glm(resp~itemp+ rt.mean,tmp,family="binomial")
    oos<-merge(oos,stud)
    oos$rt.mean<-predict(m,oos,type='response')
    ##response time plus response
    stud<-aggregate(x$resp,list(x$id),mean)
    names(stud)<-c("id","resp.mean")
    tmp<-merge(tmp,stud)
    m<-glm(resp~itemp+rt.mean+resp.mean,tmp,family="binomial")
    oos$rt.resp<-predict(m,oos,type='response')
    ##just correct
    resp1<-x[x$resp==1,]
    stud<-aggregate(resp1$rt,list(resp1$id),mean)
    names(stud)<-c("id","correct.mean")
    tmp<-merge(x,stud)
    #m<-glm(resp~factor(item)+correct.mean,tmp,family="binomial")
    m<-glm(resp~itemp+correct.mean,tmp,family="binomial")
    oos<-merge(oos,stud)
    oos$correct.mean<-predict(m,oos,type='response')
    ##
    lll$resp<-acc(oos,p='resp.mean')
    #
    lll$rt<-acc(oos,p='rt.mean')
    lll$corr<-acc(oos,p='correct.mean')
    lll$both<-acc(oos,p='rt.resp')
    #
    lll
}


dont_run<-list(
    Assistments = "assistments.Rdata", 
    `NWEA Grade 3` = "nwea_catest_longpull_Spring_3_all.Rdata", 
    `NWEA Grade 8` = "nwea_catest_longpull_Spring_8_all.Rdata"
)

library(rtmeta)
lf<-list.files(path='./3_ready/')
filenames<-filenames[unlist(filenames) %in% lf]

out<-list()
for (i in 1:length(filenames)) {
    fn<-filenames[[i]]
    print(fn)
    if (!(fn %in% unlist(dont_run))) {
        load(paste("./3_ready/",fn,sep=''))
        ####################################################################################
        set.seed(1013401) ##for repro
        out[[names(filenames)[i]]]<-oos_acc(x)
    }
}

##
#tab<-lapply(out,"[[",1)
tab<-rev(out)
tab<-lapply(tab,unlist)
tab<-do.call("cbind",tab)
offset<-0.5
z<-tab-offset
library(gplots)

#pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/acc.pdf",width=7,height=9)
par(mgp=c(2,1,0),mar=c(3,10,1,1),oma=rep(.5,4))
#cols<-c("gray","pink","red","blue","darkblue","green")
#cols<-colorRampPalette(c("royalblue", "red"))(6)
library(colorspace)
cols<-rainbow_hcl(6)
barplot2(z,beside=TRUE,horiz=TRUE,las=2,col=cols,xlim=c(0,.5),cex.names=.8,xaxt='n',xlab="0-1 Loss")
axis(side=1,at=seq(0,1,by=.1),seq(offset,1+offset,by=.1))
for (h in seq(0,1,by=.1)) abline(v=h,col='gray',lwd=.5)
rownames(z)<-c("Mean","Item mean","Response mean","RT mean","RT mean, correct","Response + RT")
legend("bottomright",bty='n',paste(rev(LETTERS[1:6]),rev(rownames(z))),fill=rev(cols),cex=.75)
#dev.off()
