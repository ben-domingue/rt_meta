##packages you'll need
#splines
#fixest
#lme4



##########################################################################################
##non-fit

library(rtmeta)
lf<-list.files(path='./3_ready/')
filenames<-filenames[unlist(filenames) %in% lf]

for (fn in filenames) {
    print(fn)
    load(paste("./3_ready/",fn,sep=''))
    prfile<-list.files(path="./4_proc",pattern=paste("proc_",fn,sep=""))
    if (length(prfile)>0) load(paste("./4_proc/proc_",fn,sep='')) else  output<-list()
    ####################################################################################
    ##descriptives
    output$desc1<-desc1(x)
    output$desc2<-desc2(x)
    ####################################################################################
    ##sat
    output$sat<-interplay(x)#,nboot=250)
    ##for fun
    ##plot(output$sat$pts,ylim=c(-.18,.18),xlim=c(-2,5),type='l')
    ##lines(output$sat$dens$`0`,col='red')
    ##lines(output$sat$dens$`1`,col='green')
    ####################################################################################
    ##gradientfield
    z<-gradfield(x)
    output$gradfield<-z
    ##for fun
    ##plot(z[,1:2],cex=.75,col=z$col,pch=19)
    ####################################################################################
    ##item
    output$item<-item_analysis(x)
    ####################################################################################
    ##person
    output$person<-person_analysis(x)
    ####################################################################################
    save(output,file=paste('./4_proc/proc_',fn,sep=''))
}

##########################################################################################
##fit

dont_run<-list(
    Assistments = "assistments.Rdata", 
    `NWEA Grade 3` = "nwea_catest_longpull_Spring_3_all.Rdata", 
    `NWEA Grade 8` = "nwea_catest_longpull_Spring_8_all.Rdata"
)

library(rtmeta)
lf<-list.files(path='./3_ready/')
filenames<-filenames[unlist(filenames) %in% lf]

out<-list()
for (fn in filenames) {
    print(fn)
    if (!(fn %in% unlist(dont_run))) {
        load(paste("./3_ready/",fn,sep=''))
        prfile<-list.files(path="./4_proc",pattern=paste("proc_",fn,sep=""))
        if (length(prfile)>0) load(paste("./4_proc/proc_",fn,sep='')) else  stop()
        ####################################################################################
        set.seed(1013401) ##for repro
        output$oos<-oos_pred(x)
       ####################################################################################
        save(output,file=paste('./4_proc/proc_',fn,sep=''))
    } else {
        load(paste("./3_ready/",fn,sep=''))
        prfile<-list.files(path="./4_proc",pattern=paste("proc_",fn,sep=""))
        if (length(prfile)>0) load(paste("./4_proc/proc_",fn,sep='')) else  stop()
        output$oos<-NULL
        save(output,file=paste('./4_proc/proc_',fn,sep=''))
    }
}

