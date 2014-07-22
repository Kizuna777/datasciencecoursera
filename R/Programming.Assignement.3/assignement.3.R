best<- function (state,outcome) {
        outcome.data<- read.table("C:/Users/selshamieh/Desktop/Assignement_3/outcome-of-care-measures.csv",
                                  header=T,
                                  na.strings ="NA", 
                                  sep=",")
        outcome.data[,c(11,17,23)] <- sapply(c(11,17,23), function(X) {
        outcome.data[,X] <- as.numeric(
                        gsub("Not Available", NA,outcome.data[,X])
                )    
        })
        outcome.p<-paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome,
                         sep=".")
        outcome.data.state<-outcome.data[grepl(state,outcome.data$State, 
                                               ignore.case=FALSE),]  
        x<-dim(outcome.data.state)  
        suppressWarnings(if (x==0) {
                stop("invalid state")
        })  
        outcome.p.col<-match(outcome.p,names(outcome.data.state)) 
        y<-outcome.p.col
        y<-is.na(y)
        if (y==TRUE) {
                stop("invalid outcome")
        }   
        minimum=min(
                min.outcome<-outcome.data.state[,outcome.p.col],
                na.rm=TRUE)
        l=outcome.data.state[outcome.data.state[,outcome.p.col]==minimum,2] 
        l.good=l[!is.na(l)]
        ses=sort(as.character(l.good), decreasing=TRUE)
        return(ses)
        
}

************************************************************************

rankhospital<- function (state,outcome,num) {
        outcome.data<- read.table("outcome-of-care-measures.csv",
                                  header=T,
                                  na.strings ="NA", 
                                  sep=",")
        
        suppressWarnings(outcome.data[,c(11:46)] <- sapply(c(11:46), function(X) {
        outcome.data[,X] <- as.numeric(
                        gsub("Not Available",
                             NA,
                             outcome.data[,X])
                ) 
        }))
        outcome.p<-paste("Hospital.30.Day.Death..Mortality..Rates.from",
                         outcome,
                         sep=".")
        outcome.data.state<-outcome.data[grepl(state,
                                               outcome.data$State, 
                                               ignore.case=FALSE),]
        x<-dim(outcome.data.state)  
        suppressWarnings(if (x==0) {
                stop("invalid state")
                })  
        outcome.p.col<-match(outcome.p,names(outcome.data.state)) 
        y<-outcome.p.col
        y<-is.na(y)
        if (y==TRUE) {
                stop("invalid outcome")
        }   
        good<-complete.cases(outcome.data.state[,outcome.p.col])
        outcome.data.state.1<-outcome.data.state[good,]
        outcome.data.state.1.2<-outcome.data.state.1[order(outcome.data.state.1$Hospital.Name),]
        l=outcome.data.state.1.2[,c("Hospital.Name",outcome.p)]
        l.1=transform(l, 
                    rankouk=ave(l[,2],
                                FUN= function(x) rank(x,
                                        ties.method="first")))
        l.2<-l.1[l.1$rankouk==num,]
        l.3<-l.2[order(l.2$rankouk),]
        if(num=="worst"){
                r=max(l.1$rankouk)     
        l.2<-l.1[l.1$rankouk==r,]
        l.3<-l.2[order(l.2$rankouk),]
        }  
        if(num=="best"){
                r=min(l.1$rankouk)     
                l.2<-l.1[l.1$rankouk==r,]
                l.3<-l.2[order(l.2$rankouk),]
        } 
        return(as.character(l.3$Hospital.Name))       
}
************************************************************************
rankall<- function (outcome,num="best") {
        outcome.data<- read.table("outcome-of-care-measures.csv",
                                  header=T,
                                  na.strings ="NA", 
                                  sep=",")
        suppressWarnings(outcome.data[,c(11:46)] <- sapply(c(11:46), function(X) {
        outcome.data[,X] <- as.numeric(
                        gsub("Not Available",
                             NA,
                             outcome.data[,X])
                )
}))
        outcome.p<-paste("Hospital.30.Day.Death..Mortality..Rates.from",
                                 outcome,
                                 sep=".")
        outcome.p.col<-match(outcome.p,names(outcome.data)) 
        y<-outcome.p.col
        y<-is.na(y)
                if (y==TRUE) {
                        stop("invalid outcome")
        } 
        outcome.data.1<-outcome.data[,c("State", "Hospital.Name", outcome.p)]
        good<-complete.cases(outcome.data.1[,outcome.p])
        outcome.data.1<-outcome.data.1[good,]
        outcome.data.1<-outcome.data.1[order(outcome.data.1$Hospital.Name),]
        outcome.data.2=transform(outcome.data.1, 
                         rankouk=ave(outcome.data.1[,3],outcome.data.1[,1],
                                     FUN= function(x) rank(x,
                                                        ties.method="first")))
        outcome.data.2.sp<-split(outcome.data.2,outcome.data.2$State) 
        ses=lapply(outcome.data.2.sp, 
                function(outcome.data.2.sp) subset(outcome.data.2.sp,
                          rankouk==num,
                          select=c("Hospital.Name","State","rankouk")))    
        ses.1<-ldply(ses,data.frame)
        if(num=="best"){
        ses=lapply(outcome.data.2.sp, 
                function(outcome.data.2.sp) subset(outcome.data.2.sp,
                                                        rankouk==1,
                                                        select=c("Hospital.Name","State","rankouk")))    
                ses.1<-ldply(ses,data.frame)
                } 
        if(num=="worst"){
        outcome.data.2=transform(outcome.data.1, 
                                 rankouk=ave(outcome.data.1[,3],outcome.data.1[,1],
                                             FUN= function(x) rank(-x,
                                                                   ties.method="first")))
        outcome.data.2.sp<-split(outcome.data.2,outcome.data.2$State) 
        ses=lapply(outcome.data.2.sp, 
                   function(outcome.data.2.sp) subset(outcome.data.2.sp,
                                                      rankouk==1,
                                                      select=c("Hospital.Name","State","rankouk")))    
        ses.1<-ldply(ses,data.frame)
        
}        

        write.table(ses.1[,1:3],file="ok.7",sep="\t")
}
******************************************************************************************************
        
