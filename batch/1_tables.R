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
