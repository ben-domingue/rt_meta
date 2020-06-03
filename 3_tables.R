setwd("/home/bd/Dropbox/projects/rt_meta/data/4_proc")
lf<-list.files(path='/home/bd/Dropbox/projects/rt_meta/data/3_ready/')
filenames<-filenames[unlist(filenames) %in% lf]

#################################################################
##table 1
ff<-function(fn) {
    load(paste("proc_",fn,sep=''))
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
    load(paste("proc_",fn,sep=''))
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
    load(paste("proc_",fn,sep=''))
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
    load(paste("proc_",fn,sep=''))
    output$oos->tab
    lapply(tab,unlist)
}
tab<-lapply(filenames,ff)
tab1<-do.call("rbind",lapply(tab,"[[",1))
tab2<-do.call("rbind",lapply(tab,"[[",2))
##
rownames(tab1)<-rownames(tab2)<-names(filenames)
tab<-list(tab1,tab2)
##
ff<-function(z) {
    library(xtable)
    z<-z[,c("base","item","irt","rt.only","rt","rt2")]
    xtable(z)
}
lapply(tab,ff)
