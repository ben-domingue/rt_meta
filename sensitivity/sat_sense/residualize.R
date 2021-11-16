interplay_resid<-function(x,std.time.in.item=FALSE,nspl=4,fe.terms = "item+id",nboot=NULL) {
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
    #first residualize time
    library(fixest)
    m <- feols(rt~1|id+item, x)
    x$rt<-resid(m)
    ##
    x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
    library(splines)
    bs(x$rt,df=nspl)->spl
    for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
    ##########################################################################
    ##now model accuracy
    modfun <- function(x, xv) {
        library(fixest)
        fm.spl <- paste(paste("spl", 1:nspl, sep = ""), collapse = "+")
        fm <- paste("resp~1+pv.center+(", fm.spl, ")", sep = "")
        fm.fe <- paste(fm, "|", fe.terms, sep = "")
        m <- feols(formula(fm.fe), x)
        fe <- fixest::fixef(m)
        M <- mean(fe$id)
        index <- which.min(abs(fe$id - M))
        id <- names(fe$id)[index]
        M <- mean(fe$item)
        index <- which.min(abs(fe$item - M)) 
       item <- names(fe$item)[index]
        pv <- 0
        tmp <- predict(spl, xv)
        for (i in 1:ncol(tmp)) colnames(tmp)[i] <- paste("spl", 
            i, sep = "")
        z <- expand.grid(pv.center = pv, rt.num = 1:nrow(tmp))
        tmp <- data.frame(rt.num = 1:100, tmp)
        z <- merge(z, tmp)
        z <- merge(z, data.frame(rt.num = 1:100, rt = xv))
        z$item <- item
        z$id <- id
        z$resp <- predict(m, z, "response")
        z$resp <- z$resp - mean(z$resp)
        pts <- cbind(z$rt, z$resp)
    }
    rt.lims <- quantile(x$rt, c(0.1, 0.9), na.rm = TRUE)
    xv <- seq(rt.lims[1], rt.lims[2], length.out = 100)
    pts <- modfun(x, xv)
    if (!is.null(nboot)) {
        pb <- list()
        for (i in 1:nboot) {
            boot.index <- sample(1:nrow(x), nrow(x), replace = TRUE)
            pb[[i]] <- modfun(x[boot.index, ], xv = xv)[, 2]
        }
        pb <- do.call("cbind", pb)
        cil <- apply(pb, 1, quantile, 0.025)
        cih <- apply(pb, 1, quantile, 0.975)
        pts <- cbind(pts, cil, cih)
    }
    xcenter <- mean(rt.lims)
    rt.lims <- c(-0.2, 0.2)
    dens <- list()
    for (resp in 0:1) {
        den <- density(x$rt[x$resp == resp])
        scale.factor <- 0.25
        m <- min(den$y)
        dy <- den$y - m
        M <- max(den$y)
        dy <- dy/M
        dy <- rt.lims[1] + scale.factor * dy * (rt.lims[2] - 
            rt.lims[1])
        dens[[as.character(resp)]] <- cbind(den$x, dy)
    }
    list(pts = pts, dens = dens)
}


fn<-'chess.Rdata'
load(paste("/home/bd/Dropbox/projects/rt_meta/data/3_ready/",fn,sep=''))
interplay_resid(x)


library(rtmeta)

L<-list()
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
for (fn in filenames) {
    output<-list()
    print(fn)
    #load(paste("/home/bd/Dropbox/projects/rt_meta/data/4_proc/proc_",fn0,sep='')) 
    load(paste("/home/bd/Dropbox/projects/rt_meta/data/3_ready/",fn,sep=''))
    x$rt<-std(x$rt)
    output$sat<-interplay(x)#,nboot=250)
    output$sat.resid<-interplay_resid(x)
    L[[fn]]<-output
}



library(rtmeta)
pdf("/home/bd/Dropbox/Apps/Overleaf/Variation in the speed-accuracy tradeoff/SI/sat_resid.pdf",width=7,height=9)
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
    lines(tmp$sat.resid$pts,col='blue',lty=1,lwd=2.5)
    #
    mtext(side=3,line=0,nm,cex=.8)
}
plot(NULL,xlim=0:1,ylim=0:1,xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
legend("topright",lwd=c(1,2.5),c("Standard","Residualized"),bty='n',cex=.7,col=c("black","blue"))
dev.off()


