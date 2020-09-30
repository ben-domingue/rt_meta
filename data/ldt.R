x<-read.csv("LDT_alldata_long.csv")
x$rt<-log(x$rt)
x$resp<-x$acc
x$item<-x$word
x$id<-x$subj

x<-x[,c("id","item","resp","rt")]
save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_ldt.Rdata",sep=""))
