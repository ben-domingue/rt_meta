##packages you'll need
#splines
#fixest
#lme4

lf<-list.files(path='./3_ready/')
filenames<-filenames[unlist(filenames) %in% lf]



source("/home/bd/Dropbox/projects/rt_meta/src/batch/A_desc.R")
source("/home/bd/Dropbox/projects/rt_meta/src/batch/B_sat.R")
source("/home/bd/Dropbox/projects/rt_meta/src/batch/C_gradient.R")
source("/home/bd/Dropbox/projects/rt_meta/src/batch/D_item.R")
source("/home/bd/Dropbox/projects/rt_meta/src/batch/E_person.R")
source("/home/bd/Dropbox/projects/rt_meta/src/batch/F_oos.R")

fitresults<-TRUE
meth.flag<-list( ##need for fit analyses
    "rr98_accuracy.Rdata"=TRUE,
    "hf_long_m2t.Rdata"=TRUE,
    "assistments.Rdata"=TRUE,
    "vdl_long_sim.Rdata"=FALSE, 
    "DIFFirt_long_sim.Rdata"=FALSE,
    "abcd.Rdata"=FALSE,
    "abcd_group.Rdata"=FALSE,
    "dd_rotation.Rdata"=FALSE, 
    "set.Rdata"=FALSE,
    "letterchaos.Rdata"=FALSE,
    "add.subtract.Rdata"=FALSE,
    "multiply.divide.Rdata"=FALSE, 
    "chess.Rdata"=FALSE,
    "piaac.Rdata"=FALSE,
    "pisa_sample.Rdata"=FALSE, 
    "nwea_catest_longpull_Spring_3_all.Rdata"=FALSE,
    "nwea_catest_longpull_Spring_8_all.Rdata"=FALSE
)

for (fn in filenames) {
    print(fn)
    
    #setwd("/home/bd/Dropbox/projects/rt_meta/data/3_ready/")
    load(paste("./3_ready/",fn,sep=''))
    prfile<-list.files(path="./4_proc",pattern=paste("proc_",fn,sep=""))
    if (length(prfile)>0) load(paste("./4_proc/proc_",fn,sep='')) else  output<-list()
    
    if (!fitresults) {
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
        
        
    }
    if (fitresults) {
        ####################################################################################
        ##fit
        mf<-meth.flag[[fn]]
        output$oos<-oos_pred(x,pv.lmer=mf)
    }
    
    ####################################################################################
    save(output,file=paste('./4_proc/proc_',fn,sep=''))
}
