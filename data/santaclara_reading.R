x1<-read.csv("SEA_combined_readComprehension_byItems_2020-08-05.csv")
x2<-read.csv("SEA_combined_readFluency_byItems_2020-08-05.csv")

ff<-function(x) {
    x$id<-paste(x$id,x$timepoint)
    x1<-x1[,c("id","item","resp","rt")]
    x2<-x2[,c("id","item","resp","rt")]
    id<-paste(x$id,x$item)
    x<-x[!duplicated(id),]
    x<-x[rowSums(is.na(x))==0,]
    x
}
x<-ff(x1)
save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_santaclara_readcomprehension.Rdata")
x<-ff(x2)
save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_santaclara_readfluency.Rdata")


