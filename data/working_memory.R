x<-readRDS("2019f_wm_ben.rds")
x$resp<-ifelse(x$acc,1,0)
x$rt<-log(x$rt)

x$item<-x$seq_length
x<-x[,c("id","item","resp","rt")]
save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_working_memory.Rdata",sep=""))
