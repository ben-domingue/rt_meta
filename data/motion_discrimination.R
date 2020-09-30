x<-read.csv("Clean_Motion_Data.csv")
x$item<-paste(x$block,x$stim)
x<-x[x$block<=6,]
x<-x[,c("subj_idx","response","rt","item")]
names(x)<-c("id","resp","rt","item")

x$rt<-log(x$rt)

save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_motion.Rdata")
