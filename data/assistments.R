x<-read.csv("skill_builder_data.csv")

x<-x[x$original==1,]
x$resp<-x$correct
x$ms_first_response->x$rt
x$rt<-x$rt/1000
x<-x[x$rt>0,]
#x$rt<-ifelse(x$rt>60*10,NA,x$rt)
x$rt<-log(x$rt)
x$id<-x$user_id
x$item<-x$problem_id
x<-x[,c("id","item","resp","rt")]

save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_assistments.Rdata")

## id<-paste(x$user_id,x$problem_id)
## tab<-table(id)
## x<-x[id %in% names(tab)[tab==1],]
## x$id <- x$user_id
## x$item <- x$problem_id

## tab<-table(x$item)
## tab<-tab[tab>20]
## x<-x[x$item %in% names(tab),]

## tab<-table(x$id)
## tab<-tab[tab>25]
## x<-x[x$id %in% names(tab),]

## NULL->x$correct

## library(lme4)
## m<-lmer(resp~0+(1|item)+(1|id),x)#,family="binomial")

## ranef(m)$item->fe
## fe<-data.frame(item=rownames(fe),diff=-1*fe[,1])
## x<-merge(x,fe)

## re<-ranef(m)$id
## re<-data.frame(id=rownames(re),th=re[,1])
## x<-merge(x,re)



## x<-x[,c("id","item","diff","resp","th","rt")]
## x<-tmp[rowSums(is.na(x))==0,]

## x$th-x$diff -> del
## exp(del)->k
## k/(1+k)->x$pv

## save(x,file="/home/bd/Dropbox/projects/rt_meta/data/assistments.Rdata")
