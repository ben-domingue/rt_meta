##############################################################################
##QC
source("/home/bd/Dropbox/projects/rt_meta/src/batch/qc_irt/qc_irt.R")
rapid.flag<-list()
repeated.trials<-c(
    "rr98_accuracy.Rdata"
)

setwd("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main")
lf<-list.files()
for (fn in lf) {
    load(fn)
    test<- fn %in% names(rapid.flag)
    if (test) x$rapid<-x[[flag[[fn]] ]] else x$rapid<-FALSE
    x<-qc(x,repeated.trials=ifelse(fn %in% repeated.trials,TRUE,FALSE))
    x$rapid<-NULL
    save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/2_preirt/",fn,sep=''))
}

##############################################################################
##IRT
source("/home/bd/Dropbox/projects/rt_meta/src/batch/qc_irt/qc_irt.R")
setwd("/home/bd/Dropbox/projects/rt_meta/data/2_preirt")
lf<-list.files()
lmer.flag<-list(
    rr98_accuracy.Rdata=TRUE,
    ##
    raw_DIFFirt_long_sim.Rdata=FALSE,
    raw_vdl_long_sim.Rdata=FALSE
)
test<-all(lf %in% names(lmer.flag))
if (!test) stop()

for (fn in lf) {
    load(fn)
    x<-irt(x,lmer.flag=lmer.flag[[fn]])
    fn2<-gsub("^raw_","",fn)
    save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/3_ready/",fn2,sep=''))
}
