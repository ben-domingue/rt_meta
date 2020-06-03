filenames <-
list(`RR98 Accuracy` = "rr98_accuracy.Rdata", `Hearts Flowers` = "hf_long_m2t.Rdata", 
    Hierarchical = "vdl_long_sim.Rdata", MSIT = "msit.Rdata", 
    DD = "DIFFirt_long_sim.Rdata", Arithmetic = "abcd.Rdata", 
    Groupitizing = "abcd_group.Rdata", Rotation = "dd_rotation.Rdata", 
    Set = "set.Rdata", `Letter Chaos` = "letterchaos.Rdata", 
    `Add Subtract` = "add.subtract.Rdata", `Working Memory` = "working_memory.Rdata", 
    `Mult Div` = "multiply.divide.Rdata", HRS = "hrs.Rdata", 
    Chess = "chess.Rdata", PERC = "perc.Rdata", Assistments = "assistments.Rdata", 
    PIAAC = "piaac.Rdata", `PISA 2015` = "pisa_sample.Rdata", 
    `PISA 2018` = "pisa2018.Rdata", `NWEA Grade 3` = "nwea_catest_longpull_Spring_3_all.Rdata", 
    `NWEA Grade 8` = "nwea_catest_longpull_Spring_8_all.Rdata")

##organize them by rt 
setwd("/home/bd/Dropbox/projects/rt_meta/data/")
M<-numeric()
for (i in 1:length(filenames)) {
    print(filenames[i])
    load(paste("./3_ready/",filenames[[i]],sep=''))
    M[i]<-mean(x$rt,na.rm=TRUE)
}
filenames<-filenames[order(M)]
dump("filenames","")







##pisa
filenames<-list.files(pattern="^pisa*.+Rdata")
txt<-gsub("pisa_long_","",filenames)
txt<-gsub(".Rdata","",txt,fixed=TRUE)
z<-list()
for (i in 1:length(txt)) z[[txt[i] ]]<-filenames[i]
filenames<-z

##piaac
filenames<-list.files(pattern="^[Pp]rg*.+Rdata")
txt<-gsub(".Rdata","",filenames,fixed=TRUE)
z<-list()
for (i in 1:length(txt)) z[[txt[i] ]]<-filenames[i]
filenames<-z

##nwea
filenames<-list.files(pattern="^nwea*.+Rdata")
txt<-gsub(".Rdata","",filenames,fixed=TRUE)
z<-list()
for (i in 1:length(txt)) z[[txt[i] ]]<-filenames[i]
filenames<-z

##ames
filenames<-list.files(pattern="^hf_long*.+Rdata")
txt<-gsub(".Rdata","",filenames,fixed=TRUE)
z<-list()
for (i in 1:length(txt)) z[[txt[i] ]]<-filenames[i]
filenames<-z
