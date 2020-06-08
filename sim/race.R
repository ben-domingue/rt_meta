##DOI : 10.1007/ S 11336-013-9396-3
get.t<-function(psi,alpha,beta) { #see eqn 8
    ##note that i will always assume the first accumulator is the correct one (eg delta=1 for that one) and thus dispense with delta
    z0<-alpha[1]-beta+rnorm(1)
    z1<-alpha[2]+rnorm(1)
    z2<-alpha[3]+rnorm(1)
    z<-exp(c(z0,z1,z2))
    index<-which.min(z)
    t<-psi+z[index]
    resp<-index-1
    resp<-ifelse(resp==1,1,0)
    c(resp,t)
}
th<-rnorm(100) #this is beta
psi<-runif(100,1,2)
library(MASS)
alpha<-mvrnorm(10,c(0,.8,.95),matrix(c(1,.8,.6,.8,1,.85,.6,.85,1),3,3,byrow=TRUE))
L<-list()
for (i in 1:length(th)) for (j in 1:nrow(alpha)) L[[paste(i,j)]]<-c(i,j,get.t(psi[i],alpha=alpha[j,],beta=th[i]))
x<-data.frame(do.call("rbind",L))
names(x)<-c("id","item","resp","rt")



library(rtmeta)
x<-irt(x)
L<-interplay(x)#,nboot=250)
plotSAT(L,nm='',xl=range(L$pts[,1]))




##Jeff's code doesn't work

#Jeff Rouder
#5/13
#IRT with Multiple Choices
#Combined data generaton and analysis

set.seed(347)
library(MASS)
I=3 #choices, first choice is correct
J=80 #items
K=80 #people

### Design Matrices and Other Errata
map= (item-1)*I+option
Xitem=matrix(0,nrow=I*J*K,ncol=I*J)
Xitem[cbind(1:(I*J*K),map)]=1
sub0=sub
sub0[option>1]=0
Xsub=matrix(0,nrow=I*J*K,ncol=K)
Xsub[cbind(1:(I*J*K),sub0)]=-1
X=cbind(Xitem,Xsub)
crossX=crossprod(X)
Xwin=matrix(0,nrow=J*K,ncol=I)
Xwin[cbind(1:(J*K),choice)]=1
win=as.logical(as.vector(t(Xwin)))


item.r=rep(1:J,K)
sub.r=rep(1:K,each=J)

option=rep(1:I,J*K)
sub=rep(sub.r,each=I)
item=rep(item.r,each=I)

t.s2=.4
t.s2.beta=(.35)^2
t.beta=rnorm(K,0,sqrt(t.s2.beta))

t.mu.sens=.5
t.s2.sens=(.2)^2
t.sens=rnorm(J,t.mu.sens,sqrt(t.s2.sens))
t.mu.center=1
t.s2.center=(.2)^2
t.center=rnorm(J,t.mu.center,sqrt(t.s2.center))
t.s2.resp=(.2)^2
t.alpha=matrix(nrow=J,ncol=I)
for (j in 1:J) t.alpha[j,]=sort(rnorm(3,t.center[j],sqrt(t.s2.resp)),decreasing=F)
t.alpha[,1]= t.alpha[,1]-t.sens
t.alpha.m=as.vector(t(t.alpha))
t.block=c(t.alpha.m,t.beta)
t.cell=X%*%t.block
t.psi=runif(K,1,2)

order1=function(x) (order(x))[1]

z=rnorm(I*J*K,t.cell,sqrt(t.s2))
z.m=tapply(z,list(item,sub),min)
choice.m=tapply(z,list(item,sub),order1)
rt=exp(z.m[cbind(item.r,sub.r)])+t.psi[sub.r]
choice=choice.m[cbind(item.r,sub.r)]
acc=(choice==1)

dat=cbind(sub.r,item.r,choice,acc,round(rt,3))
colnames(dat)=c("sub","item","choice","acc","rt")


