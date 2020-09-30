
## library(rtmeta)
## lf<-list.files(path='./3_ready/')
## filenames<-filenames[unlist(filenames) %in% lf]

## meth.flag<-list( ##need for fit analyses
##     "rr98_accuracy.Rdata"=TRUE,
##     "hf_long_m2t.Rdata"=TRUE,
##     "msit.Rdata"=TRUE,
##     "assistments.Rdata"=TRUE,
##     "working_memory.Rdata"=TRUE,
##     "race.Rdata"=TRUE,
##     ##
##     "ecls_flanker.Rdata"=FALSE,
##     "ecls_dccs.Rdata"=FALSE,
##     "hrs.Rdata"=FALSE,
##     "vdl_long_sim.Rdata"=FALSE, 
##     "DIFFirt_long_sim.Rdata"=FALSE,
##     "abcd.Rdata"=FALSE,
##     "abcd_group.Rdata"=FALSE,
##     "dd_rotation.Rdata"=FALSE, 
##     "set.Rdata"=FALSE,
##     "letterchaos.Rdata"=FALSE,
##     "add.subtract.Rdata"=FALSE,
##     "multiply.divide.Rdata"=FALSE, 
##     "chess.Rdata"=FALSE,
##     "piaac.Rdata"=FALSE,
##     "perc.Rdata"=FALSE,
##     #"pisa_sample.Rdata"=FALSE, 
##     "pisa2018math.Rdata"=FALSE, 
##     "pisa2018read.Rdata"=FALSE, 
##     "nwea_catest_longpull_Spring_3_all.Rdata"=FALSE,
##     "nwea_catest_longpull_Spring_8_all.Rdata"=FALSE,
##     "nshap.Rdata"=FALSE,
##     "mitre.Rdata"=FALSE
## )

## filenames<-filenames[!grepl("^nwea",filenames)] ##pull nwea

## oos<-list()
## for (fn in filenames) {
##     print(fn)
##     load(paste("./3_ready/",fn,sep=''))
##     prfile<-list.files(path="./4_proc",pattern=paste("proc_",fn,sep=""))
##     if (length(prfile)>0) load(paste("./4_proc/proc_",fn,sep='')) else  stop()
##     ####################################################################################
##     ##first get standardize sample
##     if (length(unique(x$id))>=250) {
##         ids<-1
##         counter<-1
##         max.items<-10
##         max.people<-250
##         outer.x<-x
##         while (counter<=10 & length(ids)<max.people) {
##             hold<-x
##             nsamp<-NULL
##             while (length(nsamp)<max.items) { ##sample items with probability based on the percentage of completed responses
##                 np<-length(unique(x$id))
##                 tab<-table(x$item)
##                 item.na<-tab/np
##                 S<-sum(item.na)
##                 p<-cumsum(item.na)/S
##                 pick<-runif(1)
##                 pick.item<- which(pick<p)[1]
##                 if (pick>max(p)) length(p)->pick.item
##                 pick.item<-names(tab)[pick.item]
##                 nsamp<-c(nsamp,pick.item)
##                 x$resp<-ifelse(x$item==pick.item,NA,x$resp)
##             }
##             ##
##             #items<-sample(names(tab),1)
##             hold->x
##             x2<-x[x$item %in% nsamp,]
##             tab<-table(x2$id)
##             ids <- names(tab)[tab>=8]
##             print(length(ids))
##             counter<-counter+1
##         }
##         if (length(ids)>=max.people) {
##             ids<-sample(ids,max.people)
##             x2<-outer.x[outer.x$item %in% nsamp & outer.x$id %in% ids,]
##             per.resp<-nrow(x2)/(max.items*max.people)
##             ##fit
##             mf<-meth.flag[[fn]]
##             set.seed(1013401) ##for repro
##             oos[[fn]]<-c(oos_pred(x2,pv.lmer=mf),per.resp=per.resp)
##         } 
##     }
## }

## tab1<-do.call("rbind",lapply(oos,"[[",1))
## tab1<-tab1[,c("base","item","irt","rt.only.lin","rt.only","rt.lin","rt2")]
## cbind(sapply(oos,"[[",3),tab1)


## #tab2<-do.call("rbind",lapply(oos,"[[",2))
## #tab2<-tab2[,c("base","item","irt","rt.only.lin","rt.only","rt.lin","rt2")]
## ##
## #rownames(tab1)<-rownames(tab2)<-names(filenames)
## #tab<-list(tab1,tab2)

