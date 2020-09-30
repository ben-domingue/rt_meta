##based on week's code, see june 22 2020 emai from weeks
library(foreign)
d <- read.spss("MITRE3 Final.sav",F,T)
d <- d[!is.na(d$CFIT1963_Total),]

ast <- d[,seq(84,239,5)]
ast <- ast[,-c(11,22)]
ass <- d[,1606:1637]
ass <- ass[,-c(11,22)]
#3
nst <- d[,seq(247,427,5)]
nst <- nst[,-c(13,26)]
nss <- d[,1639:1675]
nss <- nss[,-c(13,26)]
##
lst <- d[,seq(438,593,5)]
lst <- lst[,-c(11,22)]
lss <- d[,1677:1708]
lss <- lss[,-c(11,22)]
##
mst <- d[,seq(606,978,12)]
mst <- mst[,-c(11,22)]
mss <- d[,1998:2029]
mss <- mss[,-c(11,22)]

ast[ast<0] <- NA
#ast[ast>80] <- NA
nst[nst<0] <- NA
nst[nst>100] <- NA
lst[lst<0] <- NA
lst[lst>100] <- NA
mst[mst<0] <- NA
mst[mst>130] <- NA

id<-1:nrow(d)
resp<-cbind(ass,nss,lss)#,mss)
rt<-cbind(ast,nst,lst)#,mst)

L<-list()
for (i in 1:ncol(resp)) {
    nm<-names(resp)[i]
    L[[nm]]<-data.frame(id=id,item=nm,resp=resp[,i],rt=rt[,i])
}
x<-data.frame(do.call("rbind",L))
log(x$rt)->x$rt

save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_mitre.Rdata",sep=""))


## x<-read.csv("MITRE3 102814 FINAL Data File.csv")
## names(x)<-tolower(names(x))

## ## library(foreign)
## ## d <- read.spss("MITRE3 102814 FINAL Data File.sav",F,T)
## ## ast <- d[,seq(84,239,5)]
## ## ast <- ast[,-c(11,22)]
## ## ass <- d[,1606:1637]
## ## ass <- ass[,-c(11,22)]

## nms <-
## c("scoredas_b1i4", "scoredas_b1i12", "scoredas_b1i6", "scoredas_b1i8", 
## "scoredas_b2i1", "scoredas_b2i4", "scoredas_b2i11", "scoredas_b2i6", 
## "scoredas_b2i8", "scoredas_b2i15", "scoredas_b2i9", "scoredas_b2i16", 
## "scoredas_b3i11", "scoredas_b3i2", "scoredas_b3i12", "scoredas_b3i6", 
## "scoredas_b3i3", "scoredas_b3i9", "scoredas_b4i4", "scoredas_b4i5", 
## "scoredas_b4i7", "scoredas_b4i8", "scoredas_b4i15", "scoredas_b4i9", 
## "scoredas_b4i10", "scoredas_b5i4", "scoredas_b5i11", "scoredas_b5i7", 
## "scoredas_b5i13", "scoredas_b5i15", "scorednst_f03i1", "scorednst_f06i4", 
## "scorednst_f03i3", "scorednn_f08i12", "scorednst_f03i5", "scorednst_f07i3", 
## "scorednst_f15i1", "scorednn_f03i8", "scorednst_f06i2", "scorednn_f08i10", 
## "scorednst_f03i2", "scorednst_f09i6", "scorednst_f08i4", "scorednst_f09i4", 
## "scorednst_f06i3", "scorednst_f01i1", "scorednst_f08i5", "scorednst_f02i1", 
## "scorednst_f04i2", "scorednn_f06i12", "scorednst_f02i4", "scorednst_f06i6", 
## "scorednst_f15i2", "scorednn_f07i8", "scorednst_f06i5", "scorednst_f08i1", 
## "scorednn_f06i7", "scorednn_f04i6", "scorednn_f03i10", "scorednn_f08i6", 
## "scorednn_f04i8", "scorednn_f02i8", "scorednst_f04i3", "scorednst_f01i4", 
## "scorednn_f08i9", "scoredls_f01i4", "scoredls_f06i2", "scoredls_f04i6", 
## "scoredls_f15i4", "scoredls_f07i3", "scoredls_f06i5", "scoredls_f04i4", 
## "scoredls_f05i5", "scoredls_f11i4", "scoredls_f08i6", "scoredls_f09i1", 
## "scoredls_f15i2", "scoredls_f11i3", "scoredls_f07i2", "scoredls_f06i3", 
## "scoredls_f09i3", "scoredls_f02i4", "scoredls_f14i3", "scoredls_f06i4", 
## "scoredls_f01i2", "scoredls_f05i3", "scoredls_f14i4", "scoredls_f08i4", 
## "scoredls_f02i3", "scoredls_f01i3", "scoredls_f05i4", "scoredls_f13i2", 
## "scoredls_f15i1", "scoredls_f11i5", "scoredls_f13i4")

## L<-list()
## for (nm in nms) {
##     resp<-x[[nm]]
##     nm.base<-gsub("scored","",nm)
##     first.char<-substr(nm.base,1,1)
##     if (first.char=='n') sep<-'' else sep<-'_'
##         t0<-paste(nm.base,'t_1',sep=sep)
##     t1<-paste(nm.base,'t_2',sep=sep)
##     t2<-paste(nm.base,'t_3',sep=sep)
##     rt1<-x[[t1]]-x[[t0]]
##     rt2<-x[[t2]]-x[[t0]]
##     rt<-rt1
##     L[[nm]]<-data.frame(id=x$v1,item=nm.base,resp=resp,rt=rt1,rt2=rt2)
## }

## x<-data.frame(do.call("rbind",L))
## table(x$rt>0)
## table(x$rt2>0)
## table(x$rt2>120)
## dim(x)
## x<-x[x$rt2>0 & x$rt2<120,]
## dim(x)

## log(x$rt2)->x$rt
## NULL->x$rt2


## save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_mitre.Rdata",sep=""))
