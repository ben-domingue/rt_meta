library(MASS)
z<-mvrnorm(1000,c(0,0),matrix(c(1,.5,.5,1),2,2))
th<-z[,1]
tau<-z[,2]
diff<-rnorm(25,mean=0,sd=.5)
L<-list()
for (i in 1:length(diff)) L[[i]]<-cbind(1:length(th),i,th,tau,diff[i])
x<-data.frame(do.call("rbind",L))
names(x)<-c("id","item","th","tau","diff")
hold<-x

par(mfrow=c(5,5),mgp=c(2,1,0),mar=c(3,3,1,1))
b1L<-seq(-.5,.5,by=.25)
b2L<-seq(-.5,.5,by=.25)
for (b1 in b1L) for (b2 in b2L)
                {
                    hold->x
                    x$delta<-rnorm(nrow(x),mean=0,sd=1)
                    ##
                    kern<-(x$th+b1*x$delta)-x$diff
                    kern<-exp(kern)
                    x$pr<-kern/(kern+1)
                    x$resp<-rbinom(nrow(x),1,x$pr)
                    x$rt<-rnorm(nrow(x),-1*x$tau+b2*x$delta,1)
                    x->df
                    ##
                    x<-df[,c("id","item","resp","rt")]
                    ##
                    library(rtmeta)
                    x<-irt(x)
                    ##
                    L<-interplay(x)#,nboot=250)
                    plotSAT(L,nm=paste('b1=',b1,'; b2=',b2,sep=''),xl=c(-3,3))
                }
