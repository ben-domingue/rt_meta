x<-read.csv("PERC.S2.csv")

items<-c(paste("t1p",1:4,sep=''),##baseline puzzle-solving ability.compute set1scoreraw = sum(t1p1stotz, t1p2stotz, t1p3stotz, t1p4stotz).
         paste("t1pp",1:3,sep=''),##effort.compute eitottime = sum(t1pp1t1_page_submit, t1pp1t2_page_submit, t1pp2t1_page_submit,t1pp2t2_page_submit, t1pp3t1_page_submit, t1pp3t2_page_submit).
         paste("t1tp",1:8,sep='') ##*resiliencecompute resilrawscore = sum(t1tp6stotz, t1tp7stotz, t1tp8stotz).
         )
resp<-x[,items]

## *puzzle scores, missing values coded incorrect.
## recode t1p1 (1=1)  (else=0) into t1p1stotz.
## recode t1p2 (3=1) (else=0) into t1p2stotz.
## recode t1p3 (6=1) (else=0) into t1p3stotz.
## recode t1p4 (6=1) (else=0) into t1p4stotz.
## recode t1pp1 (4=1)  (else=0) into t1pp1stotz.execute.
## recode t1pp2 (5=1) (else=0) into t1pp2stotz.execute.
## recode t1pp3 (1=1)  (else=0) into t1pp3stotz.execute.
## recode t1tp1 (8=1) (else=0) into t1tp1stotz.
## recode t1tp2 (5=1)  (else=0) into t1tp2stotz.
## recode t1tp3 (4=1)  (else=0) into t1tp3stotz.
## recode t1tp4 (8=1)  (else=0) into t1tp4stotz.
## recode t1tp5 (4=1) (else=0) into t1tp5stotz.
## recode t1tp6 (5=1)  (else=0) into t1tp6stotz.
## recode t1tp7 (8=1)  (else=0) into t1tp7stotz.
## recode t1tp8 (3=1) (else=0) into t1tp8stotz
key<-c(1,3,6,6,
       4,5,1,
       8,5,4,8,4,5,8,3)
for (i in 1:ncol(resp)) ifelse(resp[,i]==key[i],1,0)->resp[,i]

rtnms<-c("q76_page_submit","q77_page_submit","q78_page_submit","q79_page_submit",
  "t1pp1t1_page_submit","t1pp2t1_page_submit","t1pp2t1_page_submit_1")
rt<-paste(paste("t1tp",1:8,sep=''),'time',sep='')
rt<-x[,c(rtnms,rt)]

rowSums(is.na(resp))->rs
index<-rs==0
resp[index,]->resp
rt[index,]->rt

id<-1:nrow(resp)
L<-list()
for (i in 1:ncol(resp)) L[[i]]<-data.frame(id=id,item=i,resp=resp[,i],rt=rt[,i])
x<-data.frame(do.call("rbind",L))

x$rt<-log(x$rt)

save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_perc.Rdata",sep=""))
