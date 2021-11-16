
## readRDS("resp.rds")->resp
## readRDS("rt.rds")->rt
## resp[,c("country","stuid","test_version","base_form")]->x1
## rt[,c("country","stuid","test_version","base_form")]->x2
## L<-list()
## for (i in 4:185) {
##     tmp<-data.frame(x1,resp=resp[,i],rt=rt[,i],item=names(resp)[i])
##     L[[i]]<-tmp[!is.na(tmp$resp),]
## } 
## do.call("rbind",L)->df
## data.frame(df)->df.big

## unique(df$country)

## country<-list("Australia",
##      "Austria",
##      "Belgium",
##      "Brazil",
##      "Bulgaria",
##      "Canada", 
##      "Chile",
##      "Chinese Taipei",
##      "Colombia",
##      "Costa Rica",
##      "Croatia", 
##      "Czech Republic",
##      "Denmark",
##      "Dominican Republic",
##      "Estonia", 
##      "Finland",
##      "France",
##      "Germany",
##      "Greece",
##      "Hong Kong",
##      "Hungary", 
##      "Iceland",
##      "Ireland",
##      "Israel",
##      "Italy",
##      "Japan",
##      "Korea",
##      "Latvia", 
##      "Lithuania",
##      "Luxembourg",
##      "Macao",
##      "Mexico",
##      "Montenegro",
##      "Netherlands", 
##      "New Zealand",
##      "Norway",
##      "Peru",
##      "Poland",
##      "Portugal",
##      "Qatar", 
##      "Russian Federation",
##      "Singapore",
##      "Slovak Republic",
##      "Slovenia", 
##      "Spain"=c("Spain","Spain (Regions)_duplicated_971"),
##      "Sweden",
##      "Switzerland",
##      "Thailand",
##      "United Arab Emirates", 
##      "Tunisia", "Turkey",
##      "United Kingdom",
##      #"United States",
##      "Uruguay", 
##      "B-S-J-G (China)",
##      #"USA (Massachusetts)", 
##      #"USA (North Carolina)"
##      USA=c("USA","United States")
##      )

## for (i in 1:length(country)) if (length(country[[i]]==1)) names(country)[i]<-country[i]
                                 


## for (iii in 1:length(country)) {
##     print(iii)
##     testL<-list()
##     for (j in 1:length(country[[iii]])) testL[[j]]<-grepl(country[[iii]][j],df.big$country)
##     test<-do.call("cbind",testL)
##     test<-rowSums(test,na.rm=TRUE)
##     df<-df.big[test>0,]
##     ##
##     if (nrow(df)>10000) {
##         L<-split(df,df$item)
##         for (i in 1:length(L)) L[[i]]<-L[[i]][,c("stuid","resp")]
##         resp<-L[[1]]
##         names(resp)[2]<-names(L)[1]
##         for (i in 2:length(L)) {
##             tmp<-L[[i]]
##             names(tmp)[2]<-names(L)[i]
##             resp<-merge(resp,tmp,all=TRUE)
##         }
##         ##
##         library(mirt)
##         m<-mirt(resp[,-1],1,"Rasch")
##         th<-fscores(m)
##         stud<-data.frame(stuid=resp$stuid,th=th[,1])
##         co<-coef(m)
##         co<-do.call("rbind",co[-length(co)])
##         item<-data.frame(item=names(resp)[-1],diff=-1*co[,2])
##         ##
##         df<-merge(df,stud)
##         df<-merge(df,item)
##         ##
##         x<-df
##         rm("df")
##         ##
##         names(x)->nms
##         index<-grep("stuid",nms)
##         names(x)[index]<-"id"
##         ##
##         x$th-x$diff -> del
##         exp(del)->k
##         k/(1+k)->x$pv
##         ##
##         x$rt<-log(x$rt/1000)
##         #
##         save(x,file=paste("pisa_long_",names(country)[iii],".Rdata",sep=""))
##     }
## }
