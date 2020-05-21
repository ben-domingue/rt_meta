##packages you'll need
#splines
#fixest
#lme4

fn<-'chess.Rdata'
fn<-'vdl_long_sim.Rdata'

load(fn)
output<-list()

####################################################################################
##descriptives
source("A_desc.R")
output$desc1<-desc1(x)
output$desc2<-desc2(x)

####################################################################################
##sat
source("B_sat.R")
output$sat<-interplay(x)

##for fun
##plot(output$sat$pts,ylim=c(-.18,.18),xlim=c(-2,5),type='l')
##lines(output$sat$dens$`0`,col='red')
##lines(output$sat$dens$`1`,col='green')

####################################################################################
##gradientfield

####################################################################################
##item
source("D_item.R")
output$item<-item_analysis(x)

####################################################################################
##person
source("E_person.R")
output$person<-person_analysis(x)

####################################################################################
save(output,file=paste('proc_',fn,sep=''))
