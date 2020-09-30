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
plotSAT<-function(L,nm='',
                  tl=1000,
                  axtext=FALSE, #text on axes
                  legendtext=FALSE,
                  xl=c(-2.5,5.5)
                  )
{
    plot(NULL,xlim=xl,ylim=c(-.18,.18),xlab='',ylab='',yaxt='n')
    axis(side=2,at=c(-.1,0,.1))
    legend("topleft",bty='n',legend=nm,cex=.75)
    segments(tl,-100,tl,.1,col='gray',lwd=3)
    abline(h=0,col='gray')
    if (axtext) {
        mtext(side=1,'t',line=2,cex=1)
        mtext(side=2,'Offset to Pr(x=1)',line=2,cex=1)
    }
    resp.col<-c("firebrick1","darkorchid")
    if (legendtext) {
        legend("topright",bty='n',c("Incorrect","Correct"),title="Density, t",fill=resp.col,cex=.75)
    }
    for (resp in 0:1) {
        den<-L$dens[[as.character(resp)]]
        col<-col2rgb(resp.col[resp+1])/255
        col<-rgb(col[1],col[2],col[3],alpha=.5)
        dy<-min(den[,2])
        polygon(c(den[,1],rev(den[,1])),c(rep(dy,nrow(den)),rev(den[,2])),col=col,border=NA)
    }
    tmp<-L$pts
    lines(tmp[,1:2],col="blue",lwd=1.5)
    if (ncol(tmp)>2) {
        col<-col2rgb("blue")/255
        col<-rgb(col[1],col[2],col[3],alpha=.5)
        polygon(c(tmp[,1],rev(tmp[,1])),c(tmp[,3],rev(tmp[,4])),col=col,border=NA)
    }
}


timelimits<-c("RR98 Accuracy"=10000, "Hearts Flowers"=log(1.5), "Hierarchical"=10000, "DD"=10000, "Arithmetic"=10000, 
"Groupitizing"=10000, "Rotation"=log(7.5), "Set"=log(20), "Letter Chaos"=log(20), "Add Subtract"=log(20), 
"Mult Div"=log(20), "Chess"=log(30), "Assistments"=10000, "PIAAC"=10000, "PISA Math"=10000, "NWEA Grade 3"=10000, 
"State Test"=10000, "NWEA Grade 8"=10000,PERC=10000,MSIT=log(2.5),"Working Memory"=10000,"PISA Reading"=10000,HRS=10000,
'ECLS Flanker'=log(10),'ECLS DCCS'=log(10),'Lexical'=10000,'NSHAP'=10000,'MITRE-ETS'=log(90),
'LDT'=10000,'Motion'=log(10),"Reading Fluency"=10000,"Reading Comp"=10000
)

library(rtmeta)
pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/sat_orig.pdf",width=7,height=9)
par(mfrow=c(6,5),mar=c(2,2,1,1),oma=c(2,2,.7,.7)) 
for (i in 1:length(Le)) {
    tmp<-Le[[i]]
    nm<-paste(i,names(filenames)[i])
    tl<-as.numeric(timelimits[[names(filenames)[i] ]])
    if (i==26) axtext<-TRUE else axtext<-FALSE
    if (i==1) legendtext<-TRUE else legendtext<-FALSE
    plotSAT(tmp,nm,tl=exp(tl),axtext,legendtext,xl=c(0,max(tmp$pts[,1])))
}
dev.off()



## #################################################################
## ##figure 2
## ##to compare with log
## pdf("/tmp/sat_lin.pdf",width=6,height=4)
## par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
## ll<-list(Ll,Le)
## for (i in 1:length(Ll)) {
##     for (ii in 1:length(ll)) {
##         L<-ll[[ii]]
##         plotSAT(L[[i]],nm=names(L)[i],xl=range(L[[i]]$pts[,1]))
##     }
## }
## dev.off()


## ##for presentation
## Ll->L
## #


## timelimits<-c("RR98 Accuracy"=10000, "Hearts Flowers"=log(1.5), "Hierarchical"=10000, "DD"=10000, "Arithmetic"=10000, 
## "Groupitizing"=10000, "Rotation"=log(7.5), "Set"=log(20), "Letter Chaos"=log(20), "Add Subtract"=log(20), 
## "Mult Div"=log(20), "Chess"=log(30), "Assistments"=10000, "PIAAC"=10000, "PISA Math"=10000, "NWEA Grade 3"=10000, 
## "State Test"=10000, "NWEA Grade 8"=10000,PERC=10000,MSIT=log(2.5),"Working Memory"=10000,"PISA Reading"=10000,HRS=10000,
## 'ECLS Flanker'=log(10),'ECLS DCCS'=log(10),'Lexical'=10000,'NSHAP'=10000,'MITRE-ETS'=log(90)
## )

## pdf("/tmp/sat_lin.pdf",width=7,height=9)
## par(mfrow=c(5,5),mar=c(2,2,1,1),oma=c(2,2,.7,.7)) 
## for (i in 1:length(L)) {
##     tmp<-L[[i]]
##     nm<-names(L)[i]
##     tl<-as.numeric(timelimits[[names(filenames)[i] ]])
##     if (i==21) axtext<-TRUE else axtext<-FALSE
##     if (i==1) legendtext<-TRUE else legendtext<-FALSE
##     plotSAT(tmp,nm,100000, #exp(tl),
##             axtext,legendtext)
## }
## dev.off()
