x<-readRDS("~/Dropbox/projects/ames/data/2019f_hf_trial_clean.rds")
x[x$block=="m2t",]->x
x[!x$ar,]->x #get rid of anticipatory responses
as.numeric(x$acc)->x$resp
x[!x$to,]->x
x$shape.switch<-ifelse(x$sw %in% c("shape","both"),1,0)
x$item<-paste(x$shape.switch,x$stim_shape)
x$side.switch<-ifelse(x$sw %in% c("side","both"),1,0)


##first make response matrix
x$item<-paste(x$shape.switch,x$stim_shape,x$side.switch)
library(lme4)
m<-glmer(resp~0+item+(1|id),x,family="binomial")
fixef(m)->fe
fe<-data.frame(item=gsub("item","",names(fe)),diff=-1*fe)
x<-merge(x,fe)
re<-ranef(m)$id
re<-data.frame(id=rownames(re),th=re[,1])
x<-merge(x,re)


NULL->x$rt
x$rt2->x$rt
NULL->x$rt2
tmp<-x[,c("id","item","diff","resp","th","rt")]
x<-tmp[rowSums(is.na(tmp))==0,]

x$th-x$diff -> del
exp(del)->k
k/(1+k)->x$pv
x$rt<-log(x$rt)

save(x,file="hf_long.Rdata")

source("~/Dropbox/projects/nwea/src/bd/novel_joint_model/interplay_fun.R")
interplay(x)
interplay(x,std.time.in.item=FALSE)




