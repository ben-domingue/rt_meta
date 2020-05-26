for (bl in c("m1t","m2t")) {
    x<-readRDS("~/Dropbox/projects/ames/data/2019f_hf_trial_clean.rds")
    x[x$block==bl,]->x
    #x[!x$ar,]->x #get rid of anticipatory responses
    as.numeric(x$acc)->x$resp
    x[!x$to,]->x
    x$shape.switch<-ifelse(x$sw %in% c("shape","both"),1,0)
    x$item<-paste(x$shape.switch,x$stim_shape)
    x$side.switch<-ifelse(x$sw %in% c("side","both"),1,0)
    ##first make response matrix
    x$item<-paste(x$shape.switch,x$stim_shape,x$side.switch)
    ## library(lme4)
    ## m<-glmer(resp~0+item+(1|id),x,family="binomial")
    ## fixef(m)->fe
    ## fe<-data.frame(item=gsub("item","",names(fe)),diff=-1*fe)
    ## x<-merge(x,fe)
    ## re<-ranef(m)$id
    ## re<-data.frame(id=rownames(re),th=re[,1])
    ## x<-merge(x,re)
    x$rt2->x$rt
    NULL->x$rt2
    tmp<-x[,c("id","item","resp","rt","ar")]
    x<-tmp[rowSums(is.na(tmp))==0,]
    ## x$th-x$diff -> del
    ## exp(del)->k
    ## k/(1+k)->x$pv
    x$rt<-log(x$rt)
    print(table(table(paste(x$id,x$item))))
    save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_hf_long_",bl,".Rdata",sep=""))
}


## for (bl in c("m1t","m2t")) {
##     x<-readRDS("~/Dropbox/projects/ames/data/2019f_hf_trial_clean.rds")
##     x[x$block==bl,]->x
##     x[!x$ar,]->x #get rid of anticipatory responses
##     as.numeric(x$acc)->x$resp
##     x[!x$to,]->x
##     x$shape.switch<-ifelse(x$sw %in% c("shape","both"),1,0)
##     x$item<-paste(x$shape.switch,x$stim_shape)
##     x$side.switch<-ifelse(x$sw %in% c("side","both"),1,0)
    
    
##     ##first make response matrix
##     x$item<-paste(x$shape.switch,x$stim_shape,x$side.switch)
##     library(lme4)
##     m<-glmer(resp~0+item+(1|id),x,family="binomial")
##     fixef(m)->fe
##     fe<-data.frame(item=gsub("item","",names(fe)),diff=-1*fe)
##     x<-merge(x,fe)
##     re<-ranef(m)$id
##     re<-data.frame(id=rownames(re),th=re[,1])
##     x<-merge(x,re)
    
    
##     NULL->x$rt
##     x$rt2->x$rt
##     NULL->x$rt2
##     tmp<-x[,c("id","item","diff","resp","th","rt")]
##     x<-tmp[rowSums(is.na(tmp))==0,]
    
##     x$th-x$diff -> del
##     exp(del)->k
##     k/(1+k)->x$pv
##     x$rt<-log(x$rt)
    
##     save(x,file=paste("hf_long_",bl,".Rdata",sep=""))
## }

    



