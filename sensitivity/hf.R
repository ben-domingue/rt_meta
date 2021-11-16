library(rtmeta)
filenames<-list.files(path="./3_ready/",pattern="hf_long*.+Rdata")

tl<-c("hf_long_ft.Rdata"=1.5,"hf_long_m1t.Rdata"=1.5, "hf_long_m2t.Rdata"=1.25)
  
L<-list()
for (fn in filenames) {
    print(fn)
    load(paste("./3_ready/",fn,sep=''))
    ####################################################################################
    ##sat
    x$rt<-exp(x$rt)
    n0<-nrow(x)
    x<-x[x$rt<tl[fn],]
    n1<-nrow(x)
    print((n0-n1)/n0)
    print(summary(x$rt))
    ##
    L[[fn]]<-interplay(x)#,nboot=250)
}


pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/SI/sat_hf.pdf",width=3,height=4)
par(mfrow=c(3,1),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:length(L)) {
    txt<-gsub("nwea_catest_longpull_","",names(L)[i])
    txt<-gsub(".Rdata","",txt,fixed=TRUE)
    ##
    plotSAT(L[[i]],nm=txt,xl=c(0,2),tl[i])
}
dev.off()
