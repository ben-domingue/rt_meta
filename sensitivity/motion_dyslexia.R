x<-read.csv("Clean_Motion_Data.csv")
L<-split(x,x$dys_dx)

ff<-function(x) {
    x$item<-paste(x$block,x$stim)
    x<-x[x$block<=6,]
    x<-x[,c("subj_idx","response","rt","item")]
    names(x)<-c("id","resp","rt","item")
    x$rt<-log(x$rt)
    ############qc
    library(rtmeta)
    repeated.trials<-c("raw_motion.Rdata")
    x$rapid<-FALSE
    x<-qc(x,repeated.trials=TRUE)
    x<-irt(x,lmer.flag=TRUE)
    ##
    interplay(x)#,nboot=250)
}
out<-lapply(L,ff)

par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:length(out)) {
    plotSAT(out[[i]],nm='')
    legend("topright",bty='n',paste("dys =",names(out)[i]))
}

