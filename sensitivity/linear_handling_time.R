library(rtmeta)
lf<-list.files(path='./3_ready/')
filenames<-filenames[unlist(filenames) %in% lf]


Ll<-Le<-list()
for (fn in filenames) {
    print(fn)
    load(paste("./3_ready/",fn,sep=''))
    ##original metric
    Ll[[fn]]<-interplay(x)#,nboot=250)
    ##exponentiated
    x$rt<-exp(x$rt)
    Le[[fn]]<-interplay(x)#,nboot=250)
}



#################################################################
##figure 2


pdf("/tmp/sat_lin.pdf",width=6,height=4)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
ll<-list(Ll,Le)
for (i in 1:length(Ll)) {
    for (ii in 1:length(ll)) {
        L<-ll[[ii]]
        plot(NULL,xlim=#c(-2.5,5.5)
                      range(L[[i]]$pts[,1]),
             ylim=c(-.18,.18),xlab='',ylab='',yaxt='n')
        axis(side=2,at=c(-.1,0,.1))
        legend("topleft",bty='n',legend=names(L)[i])
        #tl<-as.numeric(timelimits[[names(filenames)[i] ]])
        #segments(tl,-100,tl,.1,col='gray',lwd=3)
        abline(h=0,col='gray')
        mtext(side=1,ifelse(ii==1,'log(t)','t'),line=2,cex=1)
        mtext(side=2,'Offset to Pr(x=1)',line=2,cex=1)
        resp.col<-c("firebrick1","darkorchid")
        if (i==1) {
            legend("bottomright",bty='n',c("Incorrect","Correct"),title="Density, log(t)",fill=resp.col,cex=.75)
        }
        for (resp in 0:1) {
            den<-L[[i]]$dens[[as.character(resp)]]
            col<-col2rgb(resp.col[resp+1])/255
            col<-rgb(col[1],col[2],col[3],alpha=.5)
            dy<-min(den[,2])
            polygon(c(den[,1],rev(den[,1])),c(rep(dy,nrow(den)),rev(den[,2])),col=col,border=NA)
        }
        tmp<-L[[i]]$pts
        lines(tmp[,1:2],col="blue",lwd=1.5)
        if (ncol(tmp)>2) {
            col<-col2rgb("blue")/255
            col<-rgb(col[1],col[2],col[3],alpha=.5)
            polygon(c(tmp[,1],rev(tmp[,1])),c(tmp[,3],rev(tmp[,4])),col=col,border=NA)
        }
    }
}
dev.off()
