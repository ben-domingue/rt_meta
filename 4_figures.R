setwd("/home/bd/Dropbox/projects/rt_meta/data")

#################################################################
##figure 1
ff<-function(fn) {
    load(paste("./4_proc/proc_",fn,sep=''))
    output$desc2
}
L<-lapply(filenames,ff)

tab<-lapply(L,function(x) x$vals)
tab<-do.call("rbind",tab)

#pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/desc.pdf",width=9,height=4.5)
pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the SAT___PNAS/desc.pdf",width=5,height=5)

##plot1
par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(2,3,1,1),oma=rep(1,4))
N<-length(filenames)
cols<-rep("blue",length(filenames))
ran<-sapply(L,function(x) range(x$den[,1]))
plot(NULL,ylim=c(-10,10),xlim=c(.5,N+.5),xaxt="n",xlab='',ylab='log(t)',bty='n')
#
abline(h=log(1),col='gray',lwd=.5)
                                        #text(.2,log(1),'1s',col="black",pos=3,cex=.5)
mtext(side=4,at=log(1),'1s',line=0,cex=.7,las=2)
abline(h=log(10),col="gray",lwd=.5)
                                        #text(.2,log(10),"10s",col="black",pos=3,cex=.5)
mtext(side=4,at=log(10),'10s',line=0,cex=.7,las=2)
abline(h=log(60),col="gray",lwd=.5)
#text(.2,log(60),"60s",col="black",pos=3,cex=.5)
mtext(side=4,at=log(60),'60s',line=0,cex=.7,las=2)
#axis(side=1,at=1:N,as.character(1:N),cex.axis=.7,gap.axis=0)
mtext(side=1,at=seq(1,N,by=2),text=seq(1,N,by=2),cex=.7)
mtext(side=3,at=seq(2,N,by=2),text=seq(2,N,by=2),cex=.7)
#boxplot(rt~test,den,pch=19,col=cols,ylab="log(t)",outcex=.5,outcol=cols,add=TRUE,xaxt='n')
for (i in 1:length(L)) {
    d<-L[[i]]$den
    M<-max(d[,2])
    .9*d[,2]/M->d[,2]
    lines((d[,2]+i),d[,1])
    col<-'lightblue'
    col<-col2rgb(col)/255
    col<-rgb(col[1],col[2],col[3],alpha=.5)
    dy<-min(d[,2])
    polygon(c(rep(i,nrow(d)),rev(i+d[,2])),c(d[,1],rev(d[,1])),col=col,border=NA)
    abline(v=i,lwd=.5,col='gray',lty=3)
}
##plot2
par(mar=c(3,3,1,10))
plot(tab[,1:2],xlab="Mean item-level accuracy",ylab="Mean item-level log(t)",cex=0,xlim=c(0,1),ylim=c(-1,5),bty="n")
text(tab[,1],tab[,2],1:nrow(tab),col=cols)
# places<-seq(min(tab[,2]),max(tab[,2]),length.out=nrow(tab))
# mtext(side=4,las=2,at=places,rev(paste(1:nrow(tab),names(filenames))),cex=.7,col='black',line=.2)
places<-seq(-1,5,length.out=ceiling(nrow(tab)/2))
mp<-ceiling(nrow(tab)/2)
mtext(side=4,las=2,at=places,rev(paste(1:nrow(tab),names(filenames)))[1:mp],cex=.7,col='black',line=5.2)
mtext(side=4,las=2,at=places[-1],rev(paste(1:nrow(tab),names(filenames)))[(mp+1):nrow(tab)],cex=.7,col='black',line=.2)
abline(h=log(1),col='black')
text(.2,log(1),'1s',col="black",pos=3,cex=.8)
abline(h=log(10),col="black")
text(.2,log(10),"10s",col="black",pos=3,cex=.8)
abline(h=log(60),col="black")
text(.2,log(60),"60s",col="black",pos=3,cex=.8)
dev.off()

#################################################################
##figure 2
ff<-function(fn) {
    load(paste("./4_proc/proc_",fn,sep=''))
    output$sat
}
L<-lapply(filenames,ff)

timelimits<-c("RR98 Accuracy"=10000, "Hearts Flowers"=log(1.5), "Hierarchical"=10000, "DD"=10000, "Arithmetic"=10000, 
"Groupitizing"=10000, "Rotation"=log(7.5), "Set"=log(20), "Letter Chaos"=log(20), "Add Subtract"=log(20), 
"Mult Div"=log(20), "Chess"=log(30), "Assistments"=10000, "PIAAC"=10000, "PISA Math"=10000, "NWEA Grade 3"=10000, 
"State Test"=10000, "NWEA Grade 8"=10000,PERC=10000,MSIT=log(2.5),"Working Memory"=10000,"PISA Reading"=10000,HRS=10000,
'ECLS Flanker'=log(10),'ECLS DCCS'=log(10),'Lexical'=10000,'NSHAP'=10000,'MITRE-ETS'=log(90),
'LDT'=10000,'Motion'=log(10),"Reading Fluency"=10000,"Reading Comp"=10000
)

#color by group
kids<-c(1,3,4,5,6,7,8,9,10,11,12,17,28)
adolescents<-c(13,14,15,16,18,21,22,27,24,29)
adults<-c(2,20,21,23,26)
older<-c(19,25)
col.groups<-list(kids,adolescents,adults,older)
line.cols<-rep('gray',length(filenames))
line.cols[kids]<-'red'
line.cols[adolescents]<-'blue'
line.cols[adults]<-'green'
line.cols[older]<-'black'

#line by task
#simple<-c(1,3,4,5,6,7,8,13,14,15,17)
#complex<-c(9,10,11,12,16,18,19,20,21:29)


## ##pnas
## library(rtmeta)
## pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the SAT___PNAS/sat.pdf",width=9,height=5)
## par(mfrow=c(3,10),mar=c(1,1,.3,.3),oma=c(3,3,.7,.7)) 
## for (i in 1:length(L)) {
##     tmp<-L[[i]]
##     nm<-paste(i,names(L)[i])
##     #if (i %in% simple) nm<-tolower(nm)
##     #if (i %in% complex) nm<-toupper(nm)
##     tl<-as.numeric(timelimits[[names(filenames)[i] ]])
##     if (i==21) axtext<-TRUE else axtext<-FALSE
##     #if (i==1) legendtext<-TRUE else legendtext<-FALSE
##     xaxt<-ifelse(axtext,'s','n')
##     plotSAT(tmp,'',tl,axtext,legendtext=FALSE,line.col=line.cols[i],
##             plot.rt.density=FALSE,xaxt=xaxt,yax=axtext,
##             lwd=2.3)
##     mtext(side=3,line=0,names(L)[i],cex=.6)
## }
## plot(NULL,xlim=0:1,ylim=0:1,xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
## #legend("topright",fill=c("red","blue","green","black","white","white"),border=NA,c("Children","Adolescents","Adults","Older Adults","simple","COMPLEX"),bty='n')
## legend("topleft",fill=c("red","blue","green","black"),border=NA,c("Children","Adolescents","Adults","Older Adults"),bty='n',cex=.63)
## dev.off()

library(rtmeta)
pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/sat.pdf",width=7,height=9)
par(mfrow=c(6,5),mar=c(2,2,1.5,.5),oma=c(2,2,.7,.7)) 
for (i in 1:length(L)) {
    tmp<-L[[i]]
    nm<-paste(i,names(L)[i])
    #if (i %in% simple) nm<-tolower(nm)
    #if (i %in% complex) nm<-toupper(nm)
    tl<-as.numeric(timelimits[[names(filenames)[i] ]])
    if (i==26) axtext<-TRUE else axtext<-FALSE
    if (i==1) legendtext<-TRUE else legendtext<-FALSE
    plotSAT(tmp,'',tl,axtext,legendtext,line.col=line.cols[i])
    mtext(side=3,line=0,names(L)[i],cex=.8)
}
plot(NULL,xlim=0:1,ylim=0:1,xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
#legend("topright",fill=c("red","blue","green","black","white","white"),border=NA,c("Children","Adolescents","Adults","Older Adults","simple","COMPLEX"),bty='n')
legend("topright",fill=c("red","blue","green","black"),border=NA,c("Children","Adolescents","Adults","Older Adults"),bty='n',cex=1.2)
dev.off()




#################################################################
##figure 3
ff<-function(fn) {
    load(paste("./4_proc/proc_",fn,sep=''))
    output$grad
}
L<-lapply(filenames,ff)
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

nn<-length(filenames)
nr<-3
nc<-10
m<-matrix(c(1:nn,rep(nn+2,nr*nc-nn)),nrow=nr,ncol=nc,byrow=TRUE)
ll<-list()
for (i in 1:ncol(m)) cbind(m[,i],m[,i])->ll[[i]]
m<-do.call("cbind",ll)
m<-cbind(m,rep(nn+1,nr))

##pnas
pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the SAT___PNAS/sat_challenge.pdf",width=9,height=5)
layout(m)
par(mgp=c(2,1,0),mar=c(1,1,.3,.3),oma=c(3,3,.7,.7))
for (i in 1:length(L)) {
    z<-L[[i]]
    #frame()
    ax<- ifelse(i==21,'s','n')
    plot(z[,1:2],col=z[,3],main='',xlab='',ylab='',xaxt=ax,yaxt=ax)
    mtext(side=3,line=0,names(L)[i],cex=.6)
    if (i==21) {
        mtext(side=2,line=2,"Pr(x=1)")
        mtext(side=1,line=2,"log(t)")
    }
}
##color legend
par(mar=c(2,0.3,2,0.3))
plot(xlim=c(-.4,1),rep(0,nrow(cols)),cols$pd,col=cols$col,pch=19,cex=.5,xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
mtext(side=1,at=0,#cols$pd[1],0,
      #las=2,
      cex=.8,format(round(cols$pd[1],2),digits=2),col=cols$col[1])
n<-nrow(cols)
mtext(side=3,at=0,#cols$pd[n],0,
                                        #las=2,
      cex=.8,paste(format(round(cols$pd[n],2),digits=2),"+",sep=""),col=cols$col[n])
text(.5,0,expression(frac(partialdiff*f,partialdiff*t)),cex=1.2)
dev.off()

## pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/sat_challenge.pdf",width=7,height=9)
## layout(m)
## par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.7,4))
## for (i in 1:length(L)) {
##     z<-L[[i]]
##     #frame()
##     plot(z[,1:2],col=z[,3],main='',xlab='',ylab='')
##     mtext(side=3,line=0,names(L)[i],cex=.8)
##     if (i==26) {
##         mtext(side=2,line=2,"Pr(x=1)")
##         mtext(side=1,line=2,"log(t)")
##     }
## }
## ##color legend
## par(mar=c(2,0.3,2,0.3))
## plot(xlim=c(-.4,1),rep(0,nrow(cols)),cols$pd,col=cols$col,pch=19,cex=.5,xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
## mtext(side=1,at=0,#cols$pd[1],0,
##       #las=2,
##       format(round(cols$pd[1],2),digits=2),col=cols$col[1])
## n<-nrow(cols)
## mtext(side=3,at=0,#cols$pd[n],0,
##       #las=2,
##       paste(format(round(cols$pd[n],2),digits=2),"+",sep=""),col=cols$col[n])
## text(.5,0,expression(frac(partialdiff*f,partialdiff*t)),cex=1.2)
## dev.off()

#################################################################
##figure 4
ff<-function(fn) {
    load(paste("./4_proc/proc_",fn,sep=''))
    output$oos
}
out<-lapply(rev(filenames),ff)
    
##
tab<-lapply(out,"[[",1)
tab<-lapply(tab,unlist)
tab<-do.call("cbind",tab)
offset<-0.3
z<-tab-offset
library(gplots)

pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/fit.pdf",width=7,height=9)
par(mgp=c(2,1,0),mar=c(3,10,1,1),oma=rep(.5,4))
#cols<-c("gray","pink","red","blue","darkblue","green")
#cols<-colorRampPalette(c("royalblue", "red"))(6)
library(colorspace)
cols<-rainbow_hcl(6)
barplot2(z,beside=TRUE,horiz=TRUE,las=2,col=cols,xlim=c(0,.6),cex.names=.8,xaxt='n',xlab="Likelihood")
axis(side=1,at=seq(0,1,by=.1),seq(offset,1+offset,by=.1))
for (h in seq(0,1,by=.1)) abline(v=h,col='gray',lwd=.5)
rownames(z)<-c("Mean","Item mean","Response mean","RT mean","RT mean, correct","Response + RT")
legend("topright",bty='n',paste(rev(LETTERS[1:6]),rev(rownames(z))),fill=rev(cols),cex=.75)
dev.off()
