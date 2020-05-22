##packages you'll need
#splines
#fixest
#lme4

#fn<-'chess.Rdata'
#fn<-'vdl_long_sim.Rdata'

setwd("/home/bd/Dropbox/projects/rt_meta/src/batch")
source("A_desc.R")
source("B_sat.R")
source("C_gradient.R")
source("D_item.R")
source("E_person.R")

for (fn in filenames) {
    print(fn)
    
    setwd("/home/bd/Dropbox/projects/rt_meta/data")
    load(fn)
    output<-list()
    
    ####################################################################################
    ##descriptives
    output$desc1<-desc1(x)
    output$desc2<-desc2(x)
    
    ####################################################################################
    ##sat
    output$sat<-interplay(x)
    
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
    save(output,file=paste('/home/bd/Dropbox/projects/rt_meta/data/proc/proc_',fn,sep=''))
}
