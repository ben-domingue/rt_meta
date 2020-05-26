library(rtdists)
rr98->x
x$resp<-ifelse(x$correct,1,0)
x$item<-paste("i",x$strength)
x$id<-paste(x$id,x$session)
#source("/home/bd/Dropbox/projects/rt_meta/src/data/long2mirt.R")
L<-split(x,x$instruction)

x<-L$accuracy
x<-x[,c("id","resp","item","rt")]
x$rt<-log(x$rt)

table(table(paste(x$id,x$item)))
      
save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_rr98_accuracy.Rdata")


## for (nm in names(L)) {
##     x<-L[[nm]]
##     library(lme4)
##     m<-glmer(resp~1+(1|item)+(1|id),x,family="binomial")
##     #m<-glmer(resp~0+strength+(1|id),x,family="binomial")
##     #ranef(m)$item->fe
##     #fe<-data.frame(item=gsub("(intercept)","",rownames(fe)),diff=-1*fe[,1])
##     strength.vals<-unique(x$strength)
##     fe<-data.frame(strength=strength.vals,diff=-1*fixef(m)*strength.vals)
    
##     x<-merge(x,fe)
##     re<-ranef(m)$id
##     re<-data.frame(id=rownames(re),th=re[,1])
##     x<-merge(x,re)
##     ##x<-long2mirt(x)
##     tmp<-x[,c("id","item","diff","resp","th","rt")]
##     x<-tmp[rowSums(is.na(tmp))==0,]
##     x$th-x$diff -> del
##     exp(del)->k
##     k/(1+k)->x$pv
##     x$rt<-log(x$rt)
##     save(x,file=paste0("rr98_",nm,".Rdata"))
## }



