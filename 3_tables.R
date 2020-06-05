setwd("/home/bd/Dropbox/projects/rt_meta/data")
lf<-list.files(path="./3_ready/")
filenames<-filenames[unlist(filenames) %in% lf]

#################################################################
##table 1
ff<-function(fn) {
    load(paste("./4_proc/proc_",fn,sep=''))
    output$desc1
}
tab<-lapply(filenames,ff)
tab<-do.call("rbind",tab)
rownames(tab)<-names(filenames)
library(xtable)
xtable(tab)

#################################################################
##table 2
ff<-function(fn) {
    load(paste("./4_proc/proc_",fn,sep=''))
    output$item
}
tab<-lapply(filenames,ff)
tab<-do.call("rbind",tab)
rownames(tab)<-names(filenames)
library(xtable)
xtable(tab)

#################################################################
##table 3
ff<-function(fn) {
    load(paste("./4_proc/proc_",fn,sep=''))
    output$person
}
tab<-lapply(filenames,ff)
tab<-do.call("rbind",tab)
rownames(tab)<-names(filenames)
library(xtable)
xtable(tab)


#################################################################
##table 4
ff<-function(fn) {
    load(paste("./4_proc/proc_",fn,sep=''))
    output$oos->tab
    lapply(tab,unlist)
}
tab<-lapply(filenames,ff)
tab1<-do.call("rbind",lapply(tab,"[[",1))
tab1<-tab1[,c("base","item","irt","rt.only.lin","rt.only","rt.lin","rt2")]
tab2<-do.call("rbind",lapply(tab,"[[",2))
tab2<-tab2[,c("base","item","irt","rt.only.lin","rt.only","rt.lin","rt2")]
##
rownames(tab1)<-rownames(tab2)<-names(filenames)
tab<-list(tab1,tab2)
##
ff<-function(z) {
    library(xtable)
    z<-z[,c("base","item","irt","rt.only.lin","rt.only","rt.lin","rt2")]
    xtable(z,digits=3)
}
lapply(tab,ff)



##
library(gplots)
par(mgp=c(2,1,0),mar=c(3,10,1,1),oma=rep(.5,4))
cols<-colorRampPalette(c("red", "blue"))(ncol(tab1))
barplot2(t(tab1),beside=TRUE,horiz=TRUE,las=2,col=cols,xlim=c(0,1))
for (h in seq(0,1,by=.1)) abline(v=h,col='gray',lwd=.5)
legend("topright",bty='n',rev(colnames(tab1)),fill=rev(cols))
