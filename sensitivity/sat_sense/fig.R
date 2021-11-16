interplay_noFE<-function(x,std.time.in.item=FALSE,nspl=4) {
    ##x needs to have columns:
    ## item [item id]
    ## id [person id]
    ## diff [item difficulty]
    ## th [person theta]
    ## pv [irt-based p-value]
    ## rt [response time in metric you want to analyze]
    ## resp [item response]
    #####################################################################
    nms<-c("item","id","diff","th","pv","rt")
    if (!(all(nms %in% names(x)))) stop("need more columns")
    ##standardizd item times within item
    if (std.time.in.item) {
        L<-split(x,x$item)
        std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
        for (i in 1:length(L)) {
            L[[i]]->y
            y$rt<-std(y$rt)
            L[[i]]<-y
        }
        x<-data.frame(do.call("rbind",L))
    }
    tmp<-x[,nms]
    x<-x[rowSums(is.na(tmp))==0,]
    #############################################################################
    library(splines)
    x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
    library(splines)
    bs(x$rt,df=nspl)->spl
    for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
    ##########################################################################
    ##now model accuracy
    modfun<-function(x,xv) {
        fm.spl<-paste(paste("spl",1:nspl,sep=""),collapse="+")
        fm<-paste("resp~1+pv.center+(",fm.spl,")",sep='')
        glm(formula(fm),x,family="binomial")->m
        ##fitted values
        pv<-0
        predict(spl,xv)->tmp
        for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
        ##
        z<-expand.grid(pv.center=pv,rt.num=1:nrow(tmp))
        tmp<-data.frame(rt.num=1:100,tmp)
        z<-merge(z,tmp)
        z<-merge(z,data.frame(rt.num=1:100,rt=xv))
        z$resp<-predict(m,z,"response")
        z$resp<-z$resp-mean(z$resp)
        pts<-cbind(z$rt,z$resp)
    }
    rt.lims<-quantile(x$rt,c(.1,.9),na.rm=TRUE)
    xv<-seq(rt.lims[1],rt.lims[2],length.out=100)
    pts<-modfun(x,xv)
    ##densities
    xcenter<-mean(rt.lims)
    c(-.2,.2)->rt.lims
    dens<-list()
    for (resp in 0:1) {
        den<-density(x$rt[x$resp==resp])
        scale.factor<-.25
        m<-min(den$y)
        dy<-den$y-m
        M<-max(den$y)
        dy<-dy/M
        dy<-rt.lims[1]+scale.factor*dy*(rt.lims[2]-rt.lims[1])
        dens[[as.character(resp)]]<-cbind(den$x,dy)
    }
    list(pts=pts,dens=dens)
}



##############################################################################
##prep for figure

library(rtmeta)
lf<-list.files(path='/home/bd/Dropbox/projects/rt_meta/data/misc/2pl/')
#filenames<-filenames[unlist(filenames) %in% lf]

L<-list()
for (fn in filenames) {
    output<-list()
    print(fn)
    #load(paste("/home/bd/Dropbox/projects/rt_meta/data/4_proc/proc_",fn0,sep='')) 
    load(paste("/home/bd/Dropbox/projects/rt_meta/data/3_ready/",fn,sep=''))
    output$sat<-interplay(x)#,nboot=250)
    output$sat.no<-interplay_noFE(x)
    ####################################################################################
    ##2pl
    fn2<-paste("2pl",fn,sep="")
    if (fn2  %in% lf) {
        load(paste("/home/bd/Dropbox/projects/rt_meta/data/misc/2pl/",fn2,sep=''))
        x$pv<-x$pv.2pl
        output$sat.2pl<-interplay(x)#,nboot=250)
        output$sat.2pl.no<-interplay_noFE(x)
    }
    L[[fn]]<-output
}



library(rtmeta)
pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/SI/sat_2pl.pdf",width=7,height=9)
par(mfrow=c(6,5),mar=c(2,2,1.5,.5),oma=c(2,2,.7,.7)) 
for (i in 1:length(L)) {
    tmp<-L[[i]]
    grep(names(L)[i],filenames)
    nm<-paste(i,names(filenames)[i])
    #if (i %in% simple) nm<-tolower(nm)
    #if (i %in% complex) nm<-toupper(nm)
    tl<-50 #tl<-as.numeric(timelimits[[names(filenames)[i] ]])
    #if (i==26) axtext<-TRUE else axtext<-FALSE
    #if (i==1) legendtext<-TRUE else legendtext<-FALSE
    plotSAT(tmp$sat,'',tl,#axtext,legendtext,
            line.col='black',plot.rt.density = FALSE,
            lty=1,lwd=1)
    lines(tmp$sat.no$pts,col='blue',lty=1,lwd=1)
    #
    if ("sat.2pl" %in% names(tmp)) {
        lines(tmp$sat.2pl$pts,col='black',lty=2,lwd=2.5)
        lines(tmp$sat.2pl.no$pts,col='blue',lty=2,lwd=2)
    }
    ##
    mtext(side=3,line=0,nm,cex=.8)
}
plot(NULL,xlim=0:1,ylim=0:1,xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
legend("topright",lty=c(1,2,1,2),lwd=c(1,2.5,1,2.5),c("Rasch, Normal","2PL, Normal","Rasch, Bernoulli","2PL, Bernoulli"),bty='n',cex=.7,col=c("black","black","blue","blue"))
dev.off()
