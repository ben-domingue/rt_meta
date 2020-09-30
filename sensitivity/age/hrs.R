## read.dat.dct <- function(dat, dct, labels.included = "no") {
##   #from http://stackoverflow.com/questions/14224321/reading-dat-and-dct-directly-from-r
##   temp <- readLines(dct)
##     temp <- temp[grepl("_column", temp)]
##     switch(labels.included,
##            yes = {
##                pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
##                classes <- c("numeric", "character", "character", "numeric", "character")
##                N <- 5
##                NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
##            },
##            no = {
##                pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
##                classes <- c("numeric", "character", "character", "numeric")
##                N <- 4
##                NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
##            })
##     metadata <- setNames(lapply(1:N, function(x) {
##         out <- gsub(pattern, paste("\\", x, sep = ""), temp)
##         out <- gsub("^\\s+|\\s+$", "", out)
##         out <- gsub('\"', "", out, fixed = TRUE)
##         class(out) <- classes[x] ; out }), NAMES)
##     metadata[["ColName"]] <- make.names(gsub("\\s", "", metadata[["ColName"]]))
##     myDF <- read.fwf(dat, widths = metadata[["ColWidth"]], 
##              col.names = metadata[["ColName"]])
##     if (labels.included == "yes") {
##         attr(myDF, "col.label") <- metadata[["ColLabel"]]
##     }
##     myDF
## }
## c(q=2018)->yrs
## 1->i
## txt1<-paste("H18PR","_R",sep='')
## read.dat.dct(dat=paste(txt1,'.da',sep=''),dct=paste(txt1,'.dct',sep=''))->df2
## as.character(as.numeric(df2$HHID)*1000+as.numeric(df2$PN))->df2$hhidpn
## names(df2)<-tolower(names(df2))
## tmp<-data.frame(hhidpn=df2$hhidpn)
## z<-paste(names(yrs)[i],"x004_r",sep='')
## birth.m<-df2[[z]]
## z<-paste(names(yrs)[i],"x067_r",sep='')
## birth.y<-df2[[z]]
## tmp$birth<-(birth.m-.5)/12+birth.y
## #
## txt1<-paste("H18A","_R",sep='')
## read.dat.dct(dat=paste(txt1,'.da',sep=''),dct=paste(txt1,'.dct',sep=''))->df2
## as.character(as.numeric(df2$HHID)*1000+as.numeric(df2$PN))->df2$hhidpn
## names(df2)<-tolower(names(df2))
## ##no proxy respondent
## z<-paste(names(yrs)[i],"a009",sep='')
## df2<-df2[!is.na(df2[[z]]) & df2[[z]]==1,]
## ##
## tmp2<-data.frame(hhidpn=df2$hhidpn)
## z<-paste(names(yrs)[i],"a500",sep='')
## iw.m<-df2[[z]]
## z<-paste(names(yrs)[i],"a501",sep='')
## iw.y<-df2[[z]]
## tmp2$iw<-(iw.m-.5)/12+iw.y
## tmp<-merge(tmp,tmp2,all=TRUE)
## tmp$age<-tmp$iw-tmp$birth


## load("../../3_ready/hrs.Rdata")
## x<-merge(x,tmp,by.x='id',by.y='hhidpn')
## x<-x[!is.na(x$age),]
## gr<-cut(x$age,c(-Inf,60,70,80,90,Inf),labels=c("<60","60-70","70-80","80-90",">90"))
## L<-split(x,gr)

## library(rtmeta)
## par(mfrow=c(2,3),mgp=c(2,1,0),mar=c(3,3,1,1))
## for (i in 1:length(L)) {
##     z<-interplay(L[[i]])
##     plotSAT(z,nm='')
##     legend("topright",bty='n',names(L)[i])
## }

