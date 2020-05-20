parfun<-function(fns) {
    l<-list()
    for (i in 1:length(fns)) {
        print(i)
        fn<-fns[i]
        load(fn)
        x<-x[x$engag=="ENGAGED",]
        log(x$rt)->x$lrt
        ##spline p-values
        x$th-x$diff -> del
        del/10 -> del
        exp(del)->k
        k/(1+k)->x$pv
        #x$sn<-x$sequence_number-1
        #x$sn<-x$sn/10
        x$id<-paste(x$student,i,sep="--")
        x<-x[,c("lrt","resp","sequence_number","diff","th","itemkey","id","pv")]
        rs<-rowSums(is.na(x))
        l[[fn]]<-x[rs==0,]
    }
    print("rbinding")
    l<-do.call("rbind",l)
    x<-data.frame(l)
    l<-NULL
    rownames(x)<-NULL
    print("done")
    ##
    x$item<-x$itemkey
    x$itemkey<-NULL
    ##
    items<-list(all=1:100,
            early=1:10,
            late=30:100)
    ##all early or late
    L<-list()
    for (j in 1:length(items)) {
        xloc<-x[x$sequence_number %in% items[[j]],]
        ##sampling
        ids<-unique(xloc$id)
        len<-length(ids)
        if (len>50000) {
            ids<-sample(ids,50000)
            xloc<-xloc[xloc$id %in% ids,]
        }
        ##
        L[[names(items)[j]]]<-xloc
    }
    L
}


list.files(path="~/nwea/YxG/",pattern=".+__.+.Rdata")->lf
setwd("~/nwea/YxG")
strsplit(lf,".Rdata")->gr
txt<-sapply(gr,"[",1)

for (txt in c("Winter_3","Winter_8")) {
    index<-grep(txt,lf)
    fns<-lf[index]
    L<-parfun(fns)
    for (i in 1:length(L)) {
        x<-L[[i]]
        x$rt<-x$lrt
        ##
        save(x,file=paste("~/rt_meta/nwea/nwea_longpull_",txt,"_",names(L)[i],".Rdata",sep=""))
        }
}
