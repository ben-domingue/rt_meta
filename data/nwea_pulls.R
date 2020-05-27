##i do this two ways:
#1. homebrewed irt
#2. their ability/difficulty estimates

##################################################################################
##preparing for homebrewed p-values
parfun<-function(fns) {
    l<-list()
    for (i in 1:length(fns)) {
        print(i)
        fn<-fns[i]
        load(fn)
        x<-x[x$engag=="ENGAGED",]
        x$engag<- ! x$engag=="ENGAGED"
        log(x$rt)->x$lrt
        x$id<-paste(x$student,i,sep="--")
        ##
        x<-x[,c("lrt","resp","sequence_number","diff","th","itemkey","id","pv","engag")]
        x$pv -> x$pr.nwea
        NULL->x$pv->x$diff->x$th
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
        ## ids<-unique(xloc$id)
        ## len<-length(ids)
        ## if (len>50000) {
        ##     ids<-sample(ids,50000)
        ##     xloc<-xloc[xloc$id %in% ids,]
        ## }
        ##
        L[[names(items)[j]]]<-xloc
    }
    L
}


list.files(path="~/nwea/YxG/",pattern=".+__.+.Rdata")->lf
setwd("~/nwea/YxG")
strsplit(lf,".Rdata")->gr
txt<-sapply(gr,"[",1)

for (txt in c("Spring_3","Spring_8")) {
    index<-grep(txt,lf)
    fns<-lf[index]
    L<-parfun(fns)
    for (i in 1:length(L)) {
        x<-L[[i]]
        x$rt<-x$lrt
        ##
        save(x,file=paste("~/rt_meta/nwea/raw_nwea_longpull_",txt,"_",names(L)[i],".Rdata",sep=""))
        }
}


##################################################################################
##WITH NWEA p-value
parfun<-function(fns) {
    l<-list()
    for (i in 1:length(fns)) {
        print(i)
        fn<-fns[i]
        load(fn)
        x<-x[x$engag=="ENGAGED",]
        x$engag<- ! x$engag=="ENGAGED"
        log(x$rt)->x$lrt
        x$id<-paste(x$student,i,sep="--")
        ##
        x$th - x$diff -> kern
        kern<-exp(kern/10)
        x$pv<-kern/(1+kern)
        print(summary(x$pv))
        ##
        x<-x[,c("lrt","resp","sequence_number","diff","th","itemkey","id","pv","engag")]
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
        L[[names(items)[j]]]<-xloc
    }
    L
}


list.files(path="~/nwea/YxG/",pattern=".+__.+.Rdata")->lf
setwd("~/nwea/YxG")
strsplit(lf,".Rdata")->gr
txt<-sapply(gr,"[",1)

for (txt in c("Spring_3","Spring_8")) {
    index<-grep(txt,lf)
    fns<-lf[index]
    L<-parfun(fns)
    for (i in 1:length(L)) {
        x<-L[[i]]
        x$rt<-x$lrt
        ##
        save(x,file=paste("~/rt_meta/nwea/raw_nwea_catest_longpull_",txt,"_",names(L)[i],".Rdata",sep=""))
        }
}


#now run these through qc
#######source("/home/bd/Dropbox/projects/rt_meta/src/batch/qc_irt/qc_irt.R")

lf<-list.files(pattern="^raw.+catest.+Rdata")
rapid.flag<-list()
repeated.trials<-NULL

for (fn in lf) {
    print(fn)
    load(fn)
    test<- fn %in% names(rapid.flag)
    if (test) x$rapid<-x[[rapid.flag[[fn]] ]] else x$rapid<-FALSE
    x<-qc(x,repeated.trials=ifelse(fn %in% repeated.trials,TRUE,FALSE))
    x$rapid<-NULL
    print(dim(x))
    fn2<-gsub("^raw_","",fn)
    #save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/2_preirt/",fn,sep=''))
    save(x,file=paste("~/rt_meta/nwea/",fn2,sep='')) #ozzy
}
