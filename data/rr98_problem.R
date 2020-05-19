library(rtdists)
rr98->x
x$resp<-ifelse(x$correct,1,0)
x$item<-paste("i",x$strength)
x$id<-paste(x$id,x$session)
L<-split(x,x$instruction)
L$speed->x

library(lme4)
m<-glmer(resp~1+(1|item)+(1|id),x,family="binomial")
(ranef(m)$id) #no variation!!

