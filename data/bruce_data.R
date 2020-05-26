load(file="/home/bd/Dropbox/projects/abcd_bbk/data/all_items.Rdata")
df$item<-as.character(df$question_text)

df[df$grade %in% 3:8,]->df
df[df$year %in% c(4),]->df
df[df$module!="GROUPITIZING",]->df

df<-df[,c("item","pid","resp","rt")]
df$pid -> df$id
NULL->df$pid

df$rt<-log(df$rt/1000)
x<-df[,c("id","item","resp","rt")]
x<-x[rowSums(is.na(x))==0,]

table(table(paste(x$id,x$item)))
save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_abcd.Rdata")


## library(lme4)
## m<-glmer(resp~0+(1|item)+(1|id),x,family="binomial")
## ranef(m)$item->fe
## fe<-data.frame(item=gsub("(intercept)","",rownames(fe)),diff=-1*fe[,1])
## x<-merge(x,fe)
## re<-ranef(m)$id
## re<-data.frame(id=rownames(re),th=re[,1])
## x<-merge(x,re)


## tmp<-x[,c("id","item","diff","resp","th","rt")]
## x<-tmp[rowSums(is.na(tmp))==0,]

## x$th-x$diff -> del
## exp(del)->k
## k/(1+k)->x$pv

## save(x,file="abcd.Rdata")

#################################################################
##groupitizing
load(file="/home/bd/Dropbox/projects/abcd_bbk/data/all_items.Rdata")
df$item<-as.character(df$question_text)

df[df$grade %in% 3:8,]->df
df[df$year %in% c(4),]->df
df[df$module=="GROUPITIZING",]->df

df<-df[,c("item","pid","resp","rt")]
df$pid -> df$id
NULL->df$pid

df$rt<-log(df$rt/1000)
x<-df[,c("id","item","resp","rt")]
x<-x[rowSums(is.na(x))==0,]

table(table(paste(x$id,x$item)))
save(x,file="/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_abcd_group.Rdata")

## library(lme4)
## m<-glmer(resp~0+(1|item)+(1|id),x,family="binomial")
## ranef(m)$item->fe
## fe<-data.frame(item=gsub("(intercept)","",rownames(fe)),diff=-1*fe[,1])
## x<-merge(x,fe)
## re<-ranef(m)$id
## re<-data.frame(id=rownames(re),th=re[,1])
## x<-merge(x,re)


## tmp<-x[,c("id","item","diff","resp","th","rt")]
## x<-tmp[rowSums(is.na(tmp))==0,]

## x$th-x$diff -> del
## exp(del)->k
## k/(1+k)->x$pv


## save(x,file="abcd_group.Rdata")
