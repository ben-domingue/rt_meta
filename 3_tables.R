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
xtable(tab[,1:3],digits=0)

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
print(xtable(tab,display=c("d","d","d","d","f","f","f")))

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


