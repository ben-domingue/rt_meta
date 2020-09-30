library(rtmeta)

lf<-list.files(pattern="nwea.+",path='./3_ready/')

L<-list()
for (fn in lf) {
    print(fn)
    load(paste("./3_ready/",fn,sep=''))
    ##original metric
    L[[fn]]<-interplay(x)#,nboot=250)
}

pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/SI/sat_nwea_EarlyLate.pdf",width=6,height=4)
par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:length(L)) {
    txt<-gsub("nwea_catest_longpull_","",names(L)[i])
    txt<-gsub(".Rdata","",txt,fixed=TRUE)
    plotSAT(L[[i]],nm=txt)
}
dev.off()
