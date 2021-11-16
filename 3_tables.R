setwd("/home/bd/Dropbox/projects/rt_meta/data")
lf<-list.files(path="./3_ready/")
filenames<-filenames[unlist(filenames) %in% lf]


#################################################################
##table 1

timelimits<-c("RR98 Accuracy"=10000, "Hearts Flowers"=log(1.5), "Hierarchical"=10000, "DD"=10000, "Arithmetic"=10000, 
"Groupitizing"=10000, "Rotation"=log(7.5), "Set"=log(20), "Letter Chaos"=log(20), "Add Subtract"=log(20), 
"Mult Div"=log(20), "Chess"=log(30), "Assistments"=10000, "PIAAC"=10000, "PISA Math"=10000, "NWEA Grade 3"=10000, 
"State Test"=10000, "NWEA Grade 8"=10000,PERC=10000,MSIT=log(2.5),"Working Memory"=10000,"PISA Reading"=10000,HRS=10000,
'ECLS Flanker'=log(10),'ECLS DCCS'=log(10),'Lexical'=10000,'NSHAP'=10000,'MITRE-ETS'=log(90),
'LDT'=10000,'Motion'=log(10),"Reading Fluency"=10000,"Reading Comp"=10000
)

ff<-function(fn) {
    load(paste("./4_proc/proc_",fn,sep=''))
    c(output$desc1,exp(output$desc2$vals[2]))
}
tab<-lapply(filenames,ff)
tab<-do.call("rbind",tab)
rownames(tab)<-names(filenames)
library(xtable)

timelimits<-ifelse(timelimits>100,NA,exp(timelimits))
index<-match(rownames(tab),names(timelimits))
tab<-cbind(tab[,1:3],tab[,6],timelimits[index])

print(xtable(tab,display=c("d","d","d","d","f","f"),digits=1))

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


