
##############################################################################
##QC
library(rtmeta)
rapid.flag<-list(raw_hf_long_m2t.Rdata='ar',
                 raw_hf_long_m1t.Rdata='ar',
                 raw_hf_long_ft.Rdata='ar'
                 ##nwea stuff i removed in data prep
                 ## raw_nwea_longpull_Spring_3_all.Rdata='engag',
                 ## raw_nwea_longpull_Spring_3_early.Rdata='engag',
                 ## raw_nwea_longpull_Spring_3_late.Rdata='engag',
                 ## raw_nwea_longpull_Spring_8_all.Rdata='engag',
                 ## raw_nwea_longpull_Spring_8_early.Rdata='engag',
                 ## raw_nwea_longpull_Spring_8_late.Rdata='engag'
                 )
repeated.trials<-c(
    "raw_rr98_accuracy.Rdata",
    "raw_hf_long_m2t.Rdata",
    "raw_hf_long_m1t.Rdata",
    "raw_hf_long_ft.Rdata",
    "raw_msit.Rdata",
    "raw_working_memory.Rdata",
    "raw_race.Rdata",
    "raw_motion.Rdata"
)





setwd("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main")
lf<-list.files(pattern="*.Rdata")

for (fn in lf) {
    print(fn)
    load(fn)
    test<- fn %in% names(rapid.flag)
    if (test) x$rapid<-x[[rapid.flag[[fn]] ]] else x$rapid<-FALSE
    x<-qc(x,repeated.trials=ifelse(fn %in% repeated.trials,TRUE,FALSE))
    x$rapid<-NULL
    print(dim(x))
    save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/2_preirt/",fn,sep='')) #local
    #save(x,file=paste("~/rt_meta/nwea/pre/",fn,sep='')) #ozzy
}


##############################################################################
##IRT
library(rtmeta)
setwd("/home/bd/Dropbox/projects/rt_meta/data/2_preirt")
lf<-list.files()

lmer.flag<-list(
    raw_rr98_accuracy.Rdata=TRUE,
    raw_hf_long_m1t.Rdata=TRUE,
    raw_hf_long_m2t.Rdata=TRUE,
    raw_hf_long_ft.Rdata=TRUE,
    raw_assistments.Rdata=TRUE,
    raw_msit.Rdata=TRUE,
    raw_working_memory.Rdata=TRUE,
    raw_race.Rdata=TRUE,
    raw_motion.Rdata=TRUE
)


for (fn in lf) {
    print(fn)
    load(fn)
    test<- fn %in% names(lmer.flag)
    if (test) flag<-lmer.flag[[fn]] else flag<-FALSE
    x<-irt(x,lmer.flag=flag)
    fn2<-gsub("^raw_","",fn)
    save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/3_ready/",fn2,sep='')) #local
    #save(x,file=fn2) ##ozzy  
}

