bm<-read.table("/home/bd/Dropbox/projects/rt_meta/data/0_raw/santa_clara/SEA_Domingue_itemlables.txt",header=FALSE,fill=TRUE,sep="\t",skip=1)
bm.h<-read.table("/home/bd/Dropbox/projects/rt_meta/data/0_raw/santa_clara/SEA_Domingue_itemlables.txt",nrows=1)
names(bm)<-c("order","item","module","type","wm.label")
bm[order(bm[,1]),]->bm

bd<-read.csv("/home/bd/Dropbox/projects/rt_meta/data/0_raw/santa_clara/bruce_items.csv",header=TRUE,sep="|")
bd$type<-bm$type
bd$wm.label<-bm$wm.label
bd<-bd[bd$type!='n/a',]

##################################


load(file="/home/bd/Dropbox/projects/rt_meta/data/0_raw/santa_clara/all_items.Rdata")
df$item<-as.character(df$question_text)
NULL->df$module
dim(df)
df<-merge(df,bd)
dim(df)
paste(df$module,df$type)->df$module

df[df$grade %in% 3:8,]->df
df[df$year %in% c(4),]->df
split(df,df$module)->L

##################################

library(rtmeta)
fun<-function(df) {
    df<-df[,c("item","pid","resp","rt")]
    df$pid -> df$id
    NULL->df$pid
    df$rt<-log(df$rt/1000)
    x<-df[,c("id","item","resp","rt")]
    x<-x[rowSums(is.na(x))==0,]
    ##
    x$rapid<-FALSE
    x<-qc(x,repeated.trials=FALSE)
    x<-irt(x,lmer.flag=TRUE)
    ##
    ip<-interplay(x)#,nboot=250)
    gf<-gradfield(x)
    list(ip=ip,gf=gf)
}
out<-list()
for (i in 1:length(L)) fun(L[[i]])->out[[i]]

par(mfrow=c(4,3),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:length(out)) {
    ip<-out[[i]]$ip
    plotSAT(ip,nm=names(L)[i])
    #legend("topright",bty='n',paste("dys =",names(out)[i]))
}


####grad picture

gf<-lapply(out,function(x) x$gf)
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

nn<-length(gf)
nr<-4
nc<-3
m<-matrix(c(1:nn,rep(nn+2,nr*nc-nn)),nrow=nr,ncol=nc,byrow=TRUE)
ll<-list()
for (i in 1:ncol(m)) cbind(m[,i],m[,i])->ll[[i]]
m<-do.call("cbind",ll)
m<-cbind(m,rep(nn+1,nr))

layout(m)
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.7,4))
for (i in 1:length(gf)) {
    z<-gf[[i]]
    #frame()
    plot(z[,1:2],col=z[,3],main=names(L)[i],xlab='',ylab='')
    if (i==25) {
        mtext(side=2,line=2,"Pr(x=1)")
        mtext(side=1,line=2,"log(t)")
    }
}
##color legend
par(mar=c(2,0.3,2,0.3))
plot(xlim=c(-.4,1),rep(0,nrow(cols)),cols$pd,col=cols$col,pch=19,cex=.5,xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
mtext(side=1,at=0,#cols$pd[1],0,
      #las=2,
      format(round(cols$pd[1],2),digits=2),col=cols$col[1])
n<-nrow(cols)
mtext(side=3,at=0,#cols$pd[n],0,
      #las=2,
      paste(format(round(cols$pd[n],2),digits=2),"+",sep=""),col=cols$col[n])
text(.5,0,expression(frac(partialdiff*f,partialdiff*t)),cex=1.2)
