
library(rtmeta)
lf<-list.files(path='./3_ready/')
filenames<-filenames[unlist(filenames) %in% lf]

meth.flag<-list( ##need for fit analyses
    "rr98_accuracy.Rdata"=TRUE,
    "hf_long_m2t.Rdata"=TRUE,
    "msit.Rdata"=TRUE,
    "assistments.Rdata"=TRUE,
    "working_memory.Rdata"=TRUE,
    "race.Rdata"=TRUE,
    "motion.Rdata"=TRUE,
    ##
    "ldt.Rdata"=FALSE,
    "ecls_flanker.Rdata"=FALSE,
    "ecls_dccs.Rdata"=FALSE,
    "hrs.Rdata"=FALSE,
    "vdl_long_sim.Rdata"=FALSE, 
    "DIFFirt_long_sim.Rdata"=FALSE,
    "abcd.Rdata"=FALSE,
    "abcd_group.Rdata"=FALSE,
    "dd_rotation.Rdata"=FALSE, 
    "set.Rdata"=FALSE,
    "letterchaos.Rdata"=FALSE,
    "add.subtract.Rdata"=FALSE,
    "multiply.divide.Rdata"=FALSE, 
    "chess.Rdata"=FALSE,
    "piaac.Rdata"=FALSE,
    "perc.Rdata"=FALSE,
    #"pisa_sample.Rdata"=FALSE, 
    "pisa2018math.Rdata"=FALSE, 
    "pisa2018read.Rdata"=FALSE, 
    "nwea_catest_longpull_Spring_3_all.Rdata"=FALSE,
    "nwea_catest_longpull_Spring_8_all.Rdata"=FALSE,
    "nshap.Rdata"=FALSE,
    "mitre.Rdata"=FALSE
)

dont<-c("nwea_catest_longpull_Spring_3_all.Rdata",
        "nwea_catest_longpull_Spring_8_all.Rdata",
        "assistments.Rdata")
        

oos<-list()
for (fn in filenames) {
    if (!(fn %in% dont)) {
        print(fn)
        load(paste("./3_ready/",fn,sep=''))
        mf<-meth.flag[[fn]]
        set.seed(1013401) ##for repro
        oos[[fn]]<-oos_pred_responseLevel(x,pv.lmer=mf)
    }
}
tab<-lapply(oos,"[[",1)
tab<-lapply(tab,unlist)
tab<-do.call("rbind",tab)
tab<-data.frame(tab)

NULL->tab$base->tab$corr->tab$rt.only.lin->tab$rt.only

getcoins<-function(a) {
    f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
    nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
}
getcoins<-Vectorize(getcoins)
for (i in 1:ncol(tab)) getcoins(tab[,i])->tab[,i]

ew<-function(p1,p0) (p1-p0)/p0
tab->newtab
for (i in 1:nrow(tab)) for (j in 2:ncol(tab)) ew(p1=tab[i,j],p0=tab[i,1])->newtab[i,j]

par(mgp=c(2,1,0),mar=c(3,3,1,10),oma=rep(.5,4))
al<-c(-.05,.2)
plot(newtab[,c(2,4)],type='n',bty='n',ylim=al,xlim=al,xlab="E(W) based on response accuracy (C in F4)",ylab="E(W) based on accuracy + time (F in F4)")
cols<-ifelse(newtab[,2]<0,'red','black')
text(newtab[,2],newtab[,4],1:nrow(newtab),cex=.7,col=cols)
abline(v=0,col='gray')
abline(h=0,col='gray')
abline(0,1,col='gray',lty=2)
index<-seq(al[1],al[2],length.out=nrow(newtab))
nms<-paste(1:nrow(newtab),rownames(newtab))
for (i in 1:nrow(newtab)) mtext(side=4,nms[i],las=2,at=index[i],col=cols[i])
