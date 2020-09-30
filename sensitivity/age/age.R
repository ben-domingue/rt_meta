dat<-list()
##split by age
#
load("~/Dropbox/projects/rt_meta/data/3_ready/hrs.Rdata")
gr<-cut(x$age,c(-Inf,70,80,Inf),labels=c("Younger than 70","70-80","Older than 80"),ordered=TRUE)
#gr<-ifelse(x$age<70,"Younger than 70","70 and older")
L<-split(x,gr)
dat$HRS<-L
#
load("~/Dropbox/projects/rt_meta/data/3_ready/nshap.Rdata")
gr<-cut(x$age,c(-Inf,70,80,Inf),labels=c("Younger than 70","70-80","Older than 80"),ordered=TRUE)
#gr<-ifelse(x$age<70,"Younger than 70","70 and older")
L<-split(x,gr)
dat$NSHAP<-L
#
load("~/Dropbox/projects/rt_meta/data/3_ready/piaac.Rdata")
gr<-ifelse(x$age<12,"Younger than 70","70 and older")
L<-split(x,gr)
dat$PIAAC<-rev(L)


#########################################################################################

library(rtmeta)
pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/SI/age.pdf",width=6,height=6)
layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,7,8,8,8),nrow=3,ncol=6,byrow=TRUE))
par(mgp=c(2,1,0),mar=c(3,3,1,1))
for (ii in 1:length(dat)) {
    L<-dat[[ii]]
    for (i in 1:length(L)) {
        x<-L[[i]]
        NULL->x$th->x$diff->x$pv
        print(nrow(x))
        x$rapid<-FALSE
        x<-irt(x,lmer.flag=FALSE)
        z<-interplay(L[[i]])
        plotSAT(z,nm='')
        legend("topright",bty='n',names(L)[i])
        if (i==1) legend("topleft",bty='n',names(dat)[ii])
    }
}
dev.off()

