x<-readRDS("fmsit.rds")
x$resp<-ifelse(x$acc,1,0)
x$rt<-log(x$rt/1000)
x$id<-x$stud_id
x$item<-x$trial
x<-x[,c("id","item","resp","rt")]
save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_msit.Rdata",sep=""))
