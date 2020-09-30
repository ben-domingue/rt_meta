##this uses *response-level* response time (rather than a summary of respondent's RT) to predict an individual responose

##########################################################################################
##fit

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
    "mitre.Rdata"=FALSE,
    "santaclara_readfluency.Rdata"=FALSE,
    "santaclara_readcomprehension.Rdata"=FALSE
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

dump("oos","") ##see the bottom


#################################################################
##table 4
index<-match(names(oos),filenames)
names(oos)<-names(filenames)[index]

tab1<-do.call("rbind",lapply(oos,"[[",1))
tab1<-tab1[,c("base","item","irt","rt.only.lin","rt.only","rt.lin","rt2")]
tab2<-do.call("rbind",lapply(oos,"[[",2))
tab2<-tab2[,c("base","item","irt","rt.only.lin","rt.only","rt.lin","rt2")]
##
#rownames(tab1)<-rownames(tab2)<-names(filenames)
tab<-list(tab1,tab2)
##
ff<-function(z) {
    library(xtable)
    z<-z[,c("base","item","irt","rt.only.lin","rt.only","rt.lin","rt2")]
    xtable(z,digits=3)
}
lapply(tab,ff)

##
pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/SI/fit_si.pdf",width=6,height=5)
z<-t(tab1[,c("irt","rt.lin")])
z2<-list()
for (i in 1:ncol(z)) unlist(z[,i])->z2[[i]]
z<-do.call("cbind",z2)
colnames(z)<-rownames(tab1) #paste(1:ncol(z),rownames(tab1))
offset<-0.4
z<-z-offset
rownames(z)<-c("IRT","IRT+RT")
library(gplots)
par(mgp=c(2,1,0),mar=c(3,10,1,1),oma=rep(.5,4))
cols<-c("gray","blue")
barplot2(z,beside=TRUE,horiz=TRUE,las=2,col=cols,xlim=c(0,.5),cex.names=.8,xaxt='n',xlab="Likelihood")
axis(side=1,at=seq(0,.5,by=.1),seq(offset,offset+.5,by=.1))
for (h in seq(0,1,by=.1)) abline(v=h,col='gray',lwd=.5)
legend("topright",bty='n',rev(rownames(z)),fill=rev(cols),cex=.7)
dev.off()

