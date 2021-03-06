
filenames <-
list(Lexical = "race.Rdata", `RR98 Accuracy` = "rr98_accuracy.Rdata", 
    `Hearts Flowers` = "hf_long_m2t.Rdata", LDT = "ldt.Rdata", 
    `ECLS Flanker` = "ecls_flanker.Rdata", `ECLS DCCS` = "ecls_dccs.Rdata", 
    Motion = "motion.Rdata", MSIT = "msit.Rdata", `Reading Fluency` = "santaclara_readfluency.Rdata", 
    `Reading Comp` = "santaclara_readcomprehension.Rdata", Arithmetic = "abcd.Rdata", 
    Groupitizing = "abcd_group.Rdata", Rotation = "dd_rotation.Rdata", 
    Set = "set.Rdata", `Letter Chaos` = "letterchaos.Rdata", 
    `Add Subtract` = "add.subtract.Rdata", `Working Memory` = "working_memory.Rdata", 
    `Mult Div` = "multiply.divide.Rdata", HRS = "hrs.Rdata", 
    Chess = "chess.Rdata", `PISA Reading` = "pisa2018read.Rdata", 
    PERC = "perc.Rdata", `MITRE-ETS` = "mitre.Rdata", Assistments = "assistments.Rdata", 
    NSHAP = "nshap.Rdata", PIAAC = "piaac.Rdata", `PISA Math` = "pisa2018math.Rdata", 
    `NWEA Grade 3` = "nwea_catest_longpull_Spring_3_all.Rdata", 
    `NWEA Grade 8` = "nwea_catest_longpull_Spring_8_all.Rdata")




cbind(filenames,1:length(filenames))

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
filenames<-list.files(path="./3_ready/",pattern="hf_long*.+Rdata")
txt<-gsub(".Rdata","",filenames,fixed=TRUE)
z<-list()
for (i in 1:length(txt)) z[[txt[i] ]]<-filenames[i]
filenames<-z
