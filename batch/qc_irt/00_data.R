##############################################################################
##QC
source("/home/bd/Dropbox/projects/rt_meta/src/batch/qc_irt/qc_irt.R")
rapid.flag<-list(raw_hf_long_m2t.Rdata='ar',raw_hf_long_m1t.Rdata='ar')
repeated.trials<-c(
    "raw_rr98_accuracy.Rdata",
    "raw_hf_long_m2t.Rdata",
    "raw_hf_long_m1t.Rdata"
)

setwd("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main")
lf<-list.files()

for (fn in lf) {
    print(fn)
    load(fn)
    test<- fn %in% names(rapid.flag)
    if (test) x$rapid<-x[[rapid.flag[[fn]] ]] else x$rapid<-FALSE
    x<-qc(x,repeated.trials=ifelse(fn %in% repeated.trials,TRUE,FALSE))
    x$rapid<-NULL
    print(dim(x))
    save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/2_preirt/",fn,sep=''))
}


##############################################################################
##IRT
source("/home/bd/Dropbox/projects/rt_meta/src/batch/qc_irt/qc_irt.R")
setwd("/home/bd/Dropbox/projects/rt_meta/data/2_preirt")
lf<-list.files()
lmer.flag<-list(
    raw_rr98_accuracy.Rdata=TRUE,
    raw_hf_long_m1t.Rdata=TRUE,
    raw_hf_long_m2t.Rdata=TRUE,
    raw_assistments.Rdata=TRUE,
    ##
    raw_DIFFirt_long_sim.Rdata=FALSE,
    raw_vdl_long_sim.Rdata=FALSE,
    raw_abcd_group.Rdata=FALSE,
    raw_abcd.Rdata=FALSE,
    raw_chess.Rdata=FALSE,
    raw_dd_rotation.Rdata=FALSE
)
test<-all(lf %in% names(lmer.flag))
if (!test) stop()

for (fn in lf) {
    print(fn)
    load(fn)
    x<-irt(x,lmer.flag=lmer.flag[[fn]])
    fn2<-gsub("^raw_","",fn)
    save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/3_ready/",fn2,sep=''))
}
