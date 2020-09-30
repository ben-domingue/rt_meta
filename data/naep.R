fn<-'naep.Rdata' #this is the basic name of the output file(s)

load("ResponseTime.RData")
x<-ResponseTime
x$resp<-x$ItemRawScore ##note that i'm assuming you have this column? may need to merge in data?
#x$resp<-rbinom(nrow(x),1,runif(nrow(x))) ###comment this out! i have just added for illustrative purposes
x$rt<-log(as.numeric(x$RT))
x$id<-x$studentID
x$item<-x$accessionNumber
x<-x[x$Revisit==1,]
x<-x[,c("item","id","resp","rt")]

##get rid of polytomous items
ncat<-by(x$resp,x$item,function(x) length(unique(x[!is.na(x)])))
items<-names(ncat)[ncat==2]
x<-x[x$item %in% items,]


library(rtmeta)
##qc
x$rapid<-FALSE
x<-qc(x,repeated.trials=FALSE)
x$rapid<-NULL
save(x,file=fn) 

##irt
x<-irt(x,lmer.flag=FALSE)
fn2<-gsub("^raw_","",fn)
save(x,file=fn2)


###non-fit analyses
filenames<-list(NAEP='naep.Rdata')
library(rtmeta)

for (fn in filenames) {
    print(fn)
    load(fn)
    prfile<-list.files(pattern=paste("proc_",fn,sep=""))
    if (length(prfile)>0) load(paste("proc_",fn,sep='')) else  output<-list()
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
    save(output,file=paste('proc_',fn,sep=''))
}

##########################################################################################
##fit, might take a while

meth.flag<-list( ##need for fit analyses
    "naep.Rdata"=FALSE
)

for (fn in filenames) {
    print(fn)
    load(fn)
    prfile<-list.files(pattern=paste("proc_",fn,sep=""))
    if (length(prfile)>0) load(paste("proc_",fn,sep='')) else  stop()
    ####################################################################################
    ##fit
    mf<-meth.flag[[fn]]
    set.seed(1013401) ##for repro
    output$oos<-oos_pred(x,pv.lmer=mf)
    ####################################################################################
    save(output,file=paste('proc_',fn,sep=''))
}
