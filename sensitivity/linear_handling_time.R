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
##to compare with log
pdf("/tmp/sat_lin.pdf",width=6,height=4)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
ll<-list(Ll,Le)
for (i in 1:length(Ll)) {
    for (ii in 1:length(ll)) {
        L<-ll[[ii]]
        plotSAT(L[[i]],nm=names(L)[i],xl=range(L[[i]]$pts[,1]))
    }
}
dev.off()


##for presentation
Ll->L
#
timelimits<-c("RR98 Accuracy"=10000, "Hearts Flowers"=log(1.5), "Hierarchical"=10000, "DD"=10000, "Arithmetic"=10000, 
"Groupitizing"=10000, "Rotation"=log(7.5), "Set"=log(20), "Letter Chaos"=log(20), "Add Subtract"=log(20), 
"Mult Div"=log(20), "Chess"=log(30), "Assistments"=10000, "PIAAC"=10000, "PISA 2015"=10000, "NWEA Grade 3"=10000, 
"State Test"=10000, "NWEA Grade 8"=10000,PERC=10000,MSIT=log(2.5),"Working Memory"=10000,"PISA 2018"=10000,HRS=10000,
'ECLS Flanker'=log(10),'ECLS DCCS'=log(10)
)
pdf("/tmp/sat_lin.pdf",width=7,height=9)
par(mfrow=c(6,4),mar=c(2,2,1,1),oma=c(2,2,.7,.7)) 
for (i in 1:length(L)) {
    tmp<-L[[i]]
    nm<-names(L)[i]
    tl<-as.numeric(timelimits[[names(filenames)[i] ]])
    if (i==21) axtext<-TRUE else axtext<-FALSE
    if (i==1) legendtext<-TRUE else legendtext<-FALSE
    plotSAT(tmp,nm,#exp(tl),
            axtext,legendtext)
}
dev.off()
