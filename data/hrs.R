read.dat.dct <- function(dat, dct, labels.included = "no") {
  #from http://stackoverflow.com/questions/14224321/reading-dat-and-dct-directly-from-r
  temp <- readLines(dct)
    temp <- temp[grepl("_column", temp)]
    switch(labels.included,
           yes = {
               pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
               classes <- c("numeric", "character", "character", "numeric", "character")
               N <- 5
               NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
           },
           no = {
               pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
               classes <- c("numeric", "character", "character", "numeric")
               N <- 4
               NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
           })
    metadata <- setNames(lapply(1:N, function(x) {
        out <- gsub(pattern, paste("\\", x, sep = ""), temp)
        out <- gsub("^\\s+|\\s+$", "", out)
        out <- gsub('\"', "", out, fixed = TRUE)
        class(out) <- classes[x] ; out }), NAMES)
    metadata[["ColName"]] <- make.names(gsub("\\s", "", metadata[["ColName"]]))
    myDF <- read.fwf(dat, widths = metadata[["ColWidth"]], 
             col.names = metadata[["ColName"]])
    if (labels.included == "yes") {
        attr(myDF, "col.label") <- metadata[["ColLabel"]]
    }
    myDF
}
read.dat.dct(dat="H18D_R.da",dct="H18D_R.dct")->resp
as.character(as.numeric(resp$HHID)*1000+as.numeric(resp$PN))->resp$id

library(sas7bdat)
rt<-read.sas7bdat("secd_timings.sas7bdat")
rt<-rt[rt$QDRMODE==2,] #just in-person
rt$rt<-log(rt$Time)
rt<-rt[!is.na(rt$Path),]
as.character(as.numeric(rt$HHID)*1000+as.numeric(rt$PN))->rt$id
rt$Path -> rt$item
rt<-rt[,c("id","item","rt","IgnoreMe")]

    
save.image(file='img.Rdata')

#####################################################
load("img.Rdata")
#tab<-table(x$Path)
#write.csv(cbind(names(tab),as.numeric(tab)))

L<-list()
#Come back to word list

##count.back
x<-rt[rt$item=="SecD.Cognition1.D124_",]
y<-resp$QD124
y<-ifelse(y>5,NA,y)
y<-ifelse(y==1,1,0)
y<-data.frame(id=resp$id,resp=y)
tmp<-merge(x,y)
L$countback<-tmp

##serial7
nms<-c("SecD.Cognition1.D142_",
             "SecD.Cognition1.D143_",
             "SecD.Cognition1.D144_",
             "SecD.Cognition1.D145_",
       "SecD.Cognition1.D146_")
for (i in 1:length(nms)) {
    nm<-nms[i]
    x<-rt[rt$item==nm,]
    txt<-strsplit(nm,".",fixed=TRUE)[[1]][3]
    txt<-gsub("_","",txt)
    y<-resp[[paste("Q",txt,sep='')]]
    y<-ifelse(y==100-7*i,1,0)
    print(mean(y,na.rm=TRUE))
    y<-data.frame(id=resp$id,resp=y)
    tmp<-merge(x,y)
    L[[paste("serial7.",i,sep='')]]<-tmp
}

##facts
nms<-c(date=151,day=152,year=153,week=154,paper=155,cactus=156,pres=157,vp=158)
for (i in 1:length(nms)) {
    nm<-nms[i]
    x<-rt[rt$item==paste("SecD.Cognition1.D",nm,"_",sep=''),]
    y<-resp[[paste("QD",nm,sep='')]]
    y<-ifelse(y>5,NA,y)
    y<-ifelse(y==1,1,0)
    print(mean(y,na.rm=TRUE))
    y<-data.frame(id=resp$id,resp=y)
    tmp<-merge(x,y)
    L[[names(nms)[i]]]<-tmp
}

## ##meaning
## nms<-c(repair=161,fabric=163,domestic=165,remorse=167,plagiarize=169)
## for (i in 1:length(nms)) {
##     nm<-nms[i]
##     x<-rt[rt$item==paste("SecD.Cognition1.D",nm,"_",sep=''),]
##     y<-resp[[paste("QD",nm,sep='')]]
##     y<-ifelse(y>5,NA,y)
##     y<-ifelse(y==1,1,0)
##     print(mean(y,na.rm=TRUE))
##     y<-data.frame(id=resp$id,resp=y)
##     tmp<-merge(x,y)
##     L[[names(nms)[i]]]<-tmp
## }

##analogies
nms<-c(analogy1=250,analogy2=251,analogy3=252)
for (i in 1:length(nms)) {
    nm<-nms[i]
    #x<-rt[rt$item==paste("SecD.Cognition1.D",nm,"_",sep=''),]
    ii<-grep(nms[i],rt$IgnoreMe)
    print(table(rt$item[ii]))
    x<-rt[ii,]
    y<-resp[[paste("QD",nm,sep='')]]
    y<-ifelse(y>5,NA,y)
    y<-ifelse(y==1,1,0)
    print(mean(y,na.rm=TRUE))
    y<-data.frame(id=resp$id,resp=y)
    tmp<-merge(x,y)
    L[[names(nms)[i]]]<-tmp
}

##questions
nms<-c("disease"=178,lotto=179,savings=180)
key<-list(100,4e5,c(242))
for (i in 1:length(nms)) {
    nm<-nms[i]
    x<-rt[rt$item==paste("SecD.Cognition1.D",nm,"_",sep=''),]
    y<-resp[[paste("QD",nm,sep='')]]
    y<-ifelse(y %in% key[[i]],1,0)
    print(mean(y,na.rm=TRUE))
    y<-data.frame(id=resp$id,resp=y)
    tmp<-merge(x,y)
    L[[names(nms)[i]]]<-tmp
}




########################################################################
for (i in 1:length(L)) {
    names(L)[i]->nm
    L[[i]]->z
    nm->z$item
    z->L[[i]]
}
x<-data.frame(do.call("rbind",L))
NULL->x$IgnoreMe

tab<-table(x$id)
x<-x[x$id %in% names(tab)[tab==17],]


save(x,file=paste("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_hrs.Rdata",sep=""))
