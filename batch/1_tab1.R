lf<-list.files(pattern="^proc.+")
txt<-gsub("proc_","",lf)

##table 1
ff<-function(fn) {
    load(fn)
    output$desc1
}
tab<-lapply(lf,ff)
tab<-do.call("rbind",tab)
index<-match(txt,filenames)
rownames(tab)<-names(filenames)[index]
library(xtable)
xtable(tab)
