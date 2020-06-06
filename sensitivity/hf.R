library(rtmeta)
filenames<-list.files(path="./3_ready/",pattern="hf_long*.+Rdata")

tl<-numeric()
L<-list()
for (fn in filenames) {
    print(fn)
    load(paste("./3_ready/",fn,sep=''))
    ####################################################################################
    ##sat
    x$rt<-exp(x$rt)
    tl[fn]<-max(x$rt)
    L[[fn]]<-interplay(x)#,nboot=250)
}


pdf("/tmp/sat_hf.pdf",width=3,height=4)
par(mfrow=c(3,1),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:length(L)) {
    txt<-gsub("nwea_catest_longpull_","",names(L)[i])
    txt<-gsub(".Rdata","",txt,fixed=TRUE)
    ##
    plotSAT(L[[i]],nm=txt,xl=c(0,2))#tl[i])
}
dev.off()
