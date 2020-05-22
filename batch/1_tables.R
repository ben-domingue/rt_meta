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
