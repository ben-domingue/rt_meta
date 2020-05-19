source("~/Dropbox/projects/rt_meta/src/interplay_fun.R")

pdf("~/Dropbox/project/rt_meta/meta.pdf")
par(mfcol=c(2,2),mar=c(3,3,1,1),oma=rep(1,4))


##
load("vdl_long_sim.Rdata")
interplay(x,main="Simulated, Hierarchical")
##
load(file="DIFFirt_long_sim.Rdata")
interplay(x,main="Simulated, diffIRT")
##
load("longpull_Winter_3.Rdata")
id<-unique(y$id)
id<-sample(id,50000)
x<-y[y$id %in% id,]
x$rt<-x$lrt
interplay(x,main="NWEA, Winter Grade 3")
##
load("longpull_Winter_8.Rdata")
id<-unique(y$id)
id<-sample(id,50000)
x<-y[y$id %in% id,]
x$rt<-x$lrt
interplay(x,main="NWEA, Winter Grade 8")

load("stateTest.Rdata")
interplay(x,main="State Test")
##
load(file="pisa_long_subsample.Rdata")
interplay(x,main="PISA")
##
load(file="hf_long.Rdata")
interplay(x,main="EF Task")
##
load(file="abcd.Rdata")
interplay(x,main="Arithmetic")
##
load(file="abcd_group.Rdata")
interplay(x,main="Arithmetic (higher-order)")
##
load(file="dd_rotation.Rdata")
interplay(x,main="Rotation (diffIRT)")
dev.off()

par(mfcol=c(2,2),mar=c(3,3,1,1),oma=rep(1,4))
load("rr98_accuracy.Rdata")
interplay(x,main="rr98 accuracy")
load("rr98_speed.Rdata")
interplay(x,main="rr98 speed")








