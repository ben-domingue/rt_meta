setwd("/home/bd/Dropbox/projects/rt_meta/data")

#################################################################
pdf("/tmp/sat_bydataset.pdf",width=4,height=5)
par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
ff<-function(iii,filenames) {
    fn<-filenames[[iii]]
    load(paste("./4_proc/proc_",fn,sep=''))
    L<-list(output$sat)
    ##

timelimits<-c("RR98 Accuracy"=10000, "Hearts Flowers"=log(1.5), "Hierarchical"=10000, "DD"=10000, "Arithmetic"=10000, 
"Groupitizing"=10000, "Rotation"=log(7.5), "Set"=log(20), "Letter Chaos"=log(20), "Add Subtract"=log(20), 
"Mult Div"=log(20), "Chess"=log(30), "Assistments"=10000, "PIAAC"=10000, "PISA Math"=10000, "NWEA Grade 3"=10000, 
"State Test"=10000, "NWEA Grade 8"=10000,PERC=10000,MSIT=log(2.5),"Working Memory"=10000,"PISA Reading"=10000,HRS=10000,
'ECLS Flanker'=log(10),'ECLS DCCS'=log(10),'Lexical'=10000,'NSHAP'=10000,'MITRE-ETS'=log(90),
'LDT'=10000,'Motion'=log(10),"Reading Fluency"=10000,"Reading Comp"=10000
)

    ##
    for (i in 1:length(L)) {
        xl<-range(L[[i]]$pts[,1])
        plot(NULL,xlim=xl,ylim=c(-.18,.18),xlab='',ylab='',yaxt='n')
        axis(side=2,at=c(-.1,0,.1))
        legend("topleft",bty='n',legend=fn)
        tl<-as.numeric(timelimits[[names(filenames)[iii] ]])
        segments(tl,-100,tl,.1,col='gray',lwd=3)
        abline(h=0,col='gray')
        resp.col<-c("firebrick1","darkorchid")
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
    #################################################################
    ##figure 3
    L<-list(output$grad)
    ran<-c(-.25,.25) #####these need to match the values in C_gradient.R
    cols<-seq(-6,6,length.out=5000)
    ##translate cols via the logistic
    pr<-1/(1+exp(-cols))
    pd<-pr*(ran[2]-ran[1])+ran[1]
    cols1<-colorRampPalette(c("red", "white"))(1000)
    cols2<-rev(colorRampPalette(c("blue", "white"))(1000))
    cols<-c(cols1,cols2)
    pv<-seq(0,1,length.out=length(cols))
    col.out<-rep(NA,length(pr))
    for (i in 1:length(pr)) {    
        index<-which.min(abs(pr[i]-pv))
        col.out[i]<-cols[index]
    }
    cols<-data.frame(pd=pd,col=col.out)
    for (i in 1:length(L)) {
        z<-L[[i]]
        plot(z[,1:2],xlim=xl,col=z[,3],main=names(L)[i],xlab='',ylab='')
        if (i==17) {
            mtext(side=2,line=2,"Pr(x=1)")
            mtext(side=1,line=2,"log(t)")
        }
    }
    ## ##color legend
    ## par(mar=c(2,5,.2,5))
    ## plot(cols$pd,rep(0,nrow(cols)),col=cols$col,pch=19,cex=.5,xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
    ## mtext(side=2,#cols$pd[1],0,
    ##       las=2,format(round(cols$pd[1],2),digits=2),col=cols$col[1])
    ## n<-nrow(cols)
    ## mtext(side=4,#cols$pd[n],0,
    ##       las=2,paste(format(round(cols$pd[n],2),digits=2),"+",sep=""),col=cols$col[n])
    ## mtext(side=1,expression(frac(partialdiff*f,partialdiff*t)),cex=.9,line=-1)
}
##ff(filenames[[1]])
lapply(1:length(filenames),ff,filenames=filenames)
dev.off()
