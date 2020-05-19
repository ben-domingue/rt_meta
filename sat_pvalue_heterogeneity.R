## ##just bottom
## source("~/Dropbox/projects/rt_meta/src/interplay_w_pvalue_heterogeneity.R")
## pdf("~/Dropbox/projects/rt_meta/meta_bottom_hetero.pdf")
## par(mar=c(3,3,1,1),oma=rep(1,4))

## ##
## load("vdl_long_sim.Rdata")
## sat_pvalue(std.time.in.item=TRUE,x,main="Simulated, Hierarchical")
## mtext(side=3,adj=0,line=0,'a')
## ##
## load(file="DIFFirt_long_sim.Rdata")
## sat_pvalue(std.time.in.item=TRUE,x,main="Simulated, diffIRT")
## mtext(side=3,adj=0,line=0,'b')
## ##
## load("longpull_Winter_3.Rdata")
## id<-unique(y$id)
## id<-sample(id,50000)
## x<-y[y$id %in% id,]
## x$rt<-x$lrt
## sat_pvalue(std.time.in.item=TRUE,x,main="NWEA, Winter Grade 3")
## mtext(side=3,adj=0,line=0,'c')
## ##
## load("longpull_Winter_8.Rdata")
## id<-unique(y$id)
## id<-sample(id,50000)
## x<-y[y$id %in% id,]
## x$rt<-x$lrt
## sat_pvalue(std.time.in.item=TRUE,x,main="NWEA, Winter Grade 8")
## mtext(side=3,adj=0,line=0,'d')
## ##
## load("stateTest.Rdata")
## sat_pvalue(std.time.in.item=TRUE,x,main="State Test")
## mtext(side=3,adj=0,line=0,'e')
## ##
## load(file="pisa_long_subsample.Rdata")
## sat_pvalue(std.time.in.item=TRUE,x,main="PISA")
## mtext(side=3,adj=0,line=0,'f')
## ##
## load(file="hf_long.Rdata")
## sat_pvalue(std.time.in.item=TRUE,x,main="EF Task")
## mtext(side=3,adj=0,line=0,'g')
## ##
## load(file="abcd.Rdata")
## sat_pvalue(std.time.in.item=TRUE,x,main="Arithmetic")
## mtext(side=3,adj=0,line=0,'h')
## ##
## load(file="abcd_group.Rdata")
## sat_pvalue(std.time.in.item=TRUE,x,main="Arithmetic (higher-order)")
## mtext(side=3,adj=0,line=0,'i')
## ##
## load(file="dd_rotation.Rdata")
## sat_pvalue(std.time.in.item=TRUE,x,main="Rotation (diffIRT)")
## mtext(side=3,adj=0,line=0,'j')
## dev.off()




###############################################################################################
##p-value split
source("~/Dropbox/projects/rt_meta/src/interplay_w_pvalue_heterogeneity.R")
pdf("~/Dropbox/projects/rt_meta/meta_bottom_hetero_split.pdf")
par(mar=c(3,3,1,1),oma=rep(1,4))

##
load("vdl_long_sim.Rdata")
sat_pvalue_split(std.time.in.item=TRUE,x,main="Simulated, Hierarchical")
mtext(side=3,adj=0,line=0,'a')
##
load(file="DIFFirt_long_sim.Rdata")
sat_pvalue_split(std.time.in.item=TRUE,x,main="Simulated, diffIRT")
mtext(side=3,adj=0,line=0,'b')
##
load("longpull_Winter_3.Rdata")
id<-unique(y$id)
id<-sample(id,50000)
x<-y[y$id %in% id,]
x$rt<-x$lrt
sat_pvalue_split(std.time.in.item=TRUE,x,main="NWEA, Winter Grade 3")
mtext(side=3,adj=0,line=0,'c')
##
load("longpull_Winter_8.Rdata")
id<-unique(y$id)
id<-sample(id,50000)
x<-y[y$id %in% id,]
x$rt<-x$lrt
sat_pvalue_split(std.time.in.item=TRUE,x,main="NWEA, Winter Grade 8")
mtext(side=3,adj=0,line=0,'d')
##
load("stateTest.Rdata")
sat_pvalue_split(std.time.in.item=TRUE,x,main="State Test")
mtext(side=3,adj=0,line=0,'e')
##
load(file="pisa_long_subsample.Rdata")
sat_pvalue_split(std.time.in.item=TRUE,x,main="PISA")
mtext(side=3,adj=0,line=0,'f')
##
load(file="hf_long.Rdata")
sat_pvalue_split(std.time.in.item=TRUE,x,main="EF Task")
mtext(side=3,adj=0,line=0,'g')
##
load(file="abcd.Rdata")
sat_pvalue_split(std.time.in.item=TRUE,x,main="Arithmetic")
mtext(side=3,adj=0,line=0,'h')
##
load(file="abcd_group.Rdata")
sat_pvalue_split(std.time.in.item=TRUE,x,main="Arithmetic (higher-order)")
mtext(side=3,adj=0,line=0,'i')
##
load(file="dd_rotation.Rdata")
sat_pvalue_split(std.time.in.item=TRUE,x,main="Rotation (diffIRT)")
mtext(side=3,adj=0,line=0,'j')
dev.off()
