---
title: "R.assignement.1"
author: '*Kizuna*'
date: ", July 5, 2014"
---
  

pollutantmean <- function(directory, ID, pollutant) {
        directory=paste("~/Desktop",directory,sep="/")
        files<-list.files(path=directory,pattern="\\.csv$",full.names=T) 
        fileList<-files[ID]
        all.files.data = lapply(fileList,read.csv,header=TRUE)
        DATA = do.call("rbind",all.files.data)
        mean(DATA[,pollutant],na.rm=TRUE)
}  


complete <- function(directory, ID) {
        directory=paste("~/Desktop",directory,sep="/")
        files<-list.files(path=directory,pattern="\\.csv$",full.names=T) 
        fileList<-files[ID]
        all.files.data = lapply(fileList,read.csv,header=TRUE)
        DATA = do.call("rbind",all.files.data)                
        DATA.complete=DATA[complete.cases(DATA),]
        nobs=tapply(DATA.complete$sulfate,DATA.complete$ID, length)
        ID=rownames(nobs)
        cbind(ID,nobs)
}  


corr <- function(directory, threshold = 0) {
        directory=paste("~/Desktop",directory,sep="/")
        files<-list.files(path=directory,pattern="\\.csv$",full.names=T) 
        all.files.data = lapply(files,read.csv,header=TRUE)
        DATA = do.call("rbind",all.files.data)                
        complete.data=DATA[complete.cases(DATA),]
        complete.data.thre=complete.data[threshold,]
        cor(complete.data.thre$nitrate,complete.data.thre$sulfate)
}   

