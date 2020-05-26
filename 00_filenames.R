filenames <-
list(`RR98 Accuracy` = "rr98_accuracy.Rdata", `Hearts Flowers` = "hf_long_m2t.Rdata", 
    Hierarchical = "vdl_long_sim.Rdata", DD = "DIFFirt_long_sim.Rdata", 
    Arithmetic = "abcd.Rdata", Groupitizing = "abcd_group.Rdata", 
    Rotation = "dd_rotation.Rdata", Set = "set.Rdata", `Letter Chaos` = "letterchaos.Rdata", 
    `Add Subtract` = "add.subtract.Rdata", `Mult Div` = "multiply.divide.Rdata", 
    Chess = "chess.Rdata", Assistments = "assistments.Rdata", 
    PIAAC = "Prgusap1_2017.Rdata", PISA = "pisa_sample.Rdata", 
    `NWEA Grade 3` = "nwea_longpull_Winter_3_all.Rdata",
    #`State Test` = "stateTest.Rdata", 
    `NWEA Grade 8` = "nwea_longpull_Winter_8_all.Rdata")


##organize them by rt 
M<-numeric()
for (i in 1:length(filenames)) {
    print(filenames[i])
    load(filenames[[i]])
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
