plotSAT<-function(L,nm='',
                  tl=1000,
                  axtext=FALSE, #text on axes
                  legendtext=FALSE,
                  xl=c(-2.5,5.5),
                  line.col='blue'
                  )
{
    plot(NULL,xlim=xl,ylim=c(-.18,.18),xlab='',ylab='',yaxt='n')
    axis(side=2,at=c(-.1,0,.1))
    legend("topleft",bty='n',legend=nm,cex=.75)
    segments(tl,-100,tl,.1,col='gray',lwd=3)
    abline(h=0,col='gray')
    if (axtext) {
        mtext(side=1,'log(t)',line=2,cex=1)
        mtext(side=2,'Offset to Pr(x=1)',line=2,cex=1)
    }
    resp.col<-c("firebrick1","darkorchid")
    if (legendtext) {
        legend("bottomright",bty='n',c("Incorrect","Correct"),title="Density, log(t)",fill=resp.col,cex=.75)
    }
    for (resp in 0:1) {
        den<-L$dens[[as.character(resp)]]
        col<-col2rgb(resp.col[resp+1])/255
        col<-rgb(col[1],col[2],col[3],alpha=.5)
        dy<-min(den[,2])
        polygon(c(den[,1],rev(den[,1])),c(rep(dy,nrow(den)),rev(den[,2])),col=col,border=NA)
    }
    tmp<-L$pts
    lines(tmp[,1:2],col=line.col,lwd=1.5)
    if (ncol(tmp)>2) {
        col<-col2rgb("blue")/255
        col<-rgb(col[1],col[2],col[3],alpha=.5)
        polygon(c(tmp[,1],rev(tmp[,1])),c(tmp[,3],rev(tmp[,4])),col=col,border=NA)
    }
}
