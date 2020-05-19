##just bottom
source("~/Dropbox/projects/rt_meta/src/interplay_fun.R")



par(mfrow=c(5,4),mar=c(3,3,1,1),oma=rep(1,4))
L<-list()
for (i in 1:length(filenames)) {
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
