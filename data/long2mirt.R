## long2mirt<-function(df) {
##     ids<-unique(df$id)
##     L<-split(df,df$item)
##     #for (i in 1:length(L)) L[[i]]<-L[[i]][,c("id","resp")]
##     #resp<-L[[1]]
##     #names(resp)[2]<-names(L)[1]
##     resp<-list()
##     for (i in 1:length(L)) {
##         tmp<-L[[i]]
##         index<-match(tmp$id,ids)
##         names(tmp)[2]<-names(L)[i]
##         resp<-merge(resp,tmp,all=TRUE)
##     }
##     library(mirt)
##     m<-mirt(resp[,-1],1,"Rasch")
##     th<-fscores(m)
##     stud<-data.frame(id=resp$id,th=th[,1])
##     co<-coef(m)
##     co<-do.call("rbind",co[-length(co)])
##     item<-data.frame(item=names(resp)[-1],diff=-1*co[,2])
##     ##
##     df<-merge(df,stud)
##     df<-merge(df,item)
##     df
## }
