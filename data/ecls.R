x<-read.csv("childK5p_Flankers.csv")
x$id<-x$CHILDID
x$item<-x$itemid_flankers
x$resp<-ifelse(x$C8FLKACC<0,NA,x$C8FLKACC)
x$resp<-ifelse(x$resp==2,0,x$resp)
x$C8FLKRT<-ifelse(x$C8FLKRT<=0,NA,x$C8FLKRT)
x$rt<-log(x$C8FLKRT/1000)
x<-x[,c("id","item","resp","rt")]
x<-x[rowSums(is.na(x))==0,]
#print(table(table(paste(x$id,x$item))))
summary(x$rt)
save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_ecls_flanker.Rdata",sep=""))

x<-read.csv("childK5p_DCCS.csv")
x$id<-x$CHILDID
x$item<-x$itemid_dccs
x$resp<-ifelse(x$C8DCCS<0,NA,x$C8DCCS)
x$resp<-ifelse(x$resp==2,0,x$resp)
x$C8TARGRT<-ifelse(x$C8TARGRT<=0,NA,x$C8TARGRT)
x$rt<-log(x$C8TARGRT/1000)
x<-x[,c("id","item","resp","rt")]
x<-x[rowSums(is.na(x))==0,]
#print(table(table(paste(x$id,x$item))))
summary(x$rt)
save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_ecls_dccs.Rdata",sep=""))

