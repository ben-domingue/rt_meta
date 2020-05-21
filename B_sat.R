##just bottom
source("~/Dropbox/projects/rt_meta/src/interplay_fun.R")

timelimits<-c("RR98 Accuracy"=10000, "Hearts Flowers"=log(1.5), "Hierarchical"=10000, "DD"=10000, "Arithmetic"=10000, 
"Groupitizing"=10000, "Rotation"=log(7.5), "Set"=10000, "Letter Chaos"=10000, "Add Subtract"=log(20), 
"Mult Div"=log(20), "Chess", "Assistments"=10000, "PIAAC"=10000, "PISA"=10000, "NWEA Grade 3"=10000, 
"State Test"=10000, "NWEA Grade 8"=10000)


##normal
pdf("/tmp/sat.pdf",width=7,height=9)
par(mfrow=c(5,4),mar=c(2,2,1,1),oma=c(2,2,.7,.7)) 
L<-list()
for (i in 1:length(filenames)) {
    load(filenames[[i]])
    ##
    interplay(std.time.in.item=FALSE,top.plot=FALSE,lm.line=FALSE,x,xlab="",xlim=c(-2.5,5.5),ylab="")
    if (i==17) {
        mtext(side=1,'log(t)',line=2)
        mtext(side=2,'Offset to Pr(x=1)',line=2)
    }
    abline(v=timelimits[names(filenames)[i] ],col='gray')
    legend("topleft",bty='n',legend=names(filenames)[i])
    if (i==4) {
        legend("topright",bty='n',c("Incorrect","Correct"),title="Density, log(t)",fill=c("red","green"),cex=1)
    }
}
dev.off()

###################################################################################

##pisa, piaac, nwea

ll<-seq(1,length(filenames),by=20)
if (ll[length(ll)]!=length(filenames)) c(ll,length(filenames)+1)->ll
                                         
for (ii in 1:(length(ll)-1)) {
    ##pisa
    #pdf(paste("/home/bd/Dropbox/projects/rt_meta/docs/si/sat_pisa_",ii,".pdf",sep=""),width=15,height=10)
    #par(mfrow=c(4,5),mar=c(3,3,1,1),oma=rep(1,4))
    ##piaac
    pdf(paste("/home/bd/Dropbox/projects/rt_meta/docs/si/sat_piaac_",ii,".pdf",sep=""),width=15,height=10)
    par(mfrow=c(4,5),mar=c(3,3,1,1),oma=rep(1,4))
    #nwea
    #pdf(paste("/home/bd/Dropbox/projects/rt_meta/docs/si/sat_nwea_",ii,".pdf",sep=""),width=15,height=10)
    #par(mfrow=c(2,3),mar=c(3,3,1,1),oma=rep(1,4))
    L<-list()
    for (i in ll[ii]:(ll[ii+1]-1)) {
        load(filenames[[i]])
        ##
        id<-unique(x$id)
        if (length(id)>50000) {
            id<-sample(id,50000)
            x<-x[x$id %in% id,]
        }
        interplay(std.time.in.item=FALSE,top.plot=FALSE,lm.line=FALSE,x,xlab='log(t)')#,main=names(filenames)[i])
        legend("topleft",bty='n',legend=names(filenames)[i])
        if (i==4) {
            legend("topright",bty='n',lty=c(1,2),c("Incorrect","Correct"),title="Density, log(t)",col='blue',cex=.75)
        }
    }
    dev.off()
}

