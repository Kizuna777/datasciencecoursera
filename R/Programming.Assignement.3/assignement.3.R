# 7ot badela best.R li 3ala pc labo taba3ak..
best<- function (state,outcome) {
        outcome.data<- read.table("~/Github/datasciencecoursera/R/Programming.Assignement.3/outcome-of-care-measures.csv",
                                  header=T,
                                  na.strings ="NA", 
                                  sep=",")
        outcome.data[,11:46] <- sapply(11:46, function(X) {
                outcome.data[,X] <- as.numeric(
                        gsub("Not Available", NA,outcome.data[,X])
                )                                                                                                        
        })
        outcome.p<-paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome,
                         sep=".")
        outcome.data.state<-outcome.data[grepl(state,outcome.data$State, ignore.case=FALSE),]  
        x<-dim(outcome.data.state)  
        if (x==0) {
                stop("invalid state")
        }        
        outcome.p.col<-match(outcome.p,names(outcome.data.state)) 
        y<-outcome.p.col
        if (y!= 11) {
                stop("invalid outcome")
        if (y!= 17) {
                        stop("invalid outcome")
                }   
        
        }   

        minimum=min(
                min.outcome<-outcome.data.state[,outcome.p.col],
                na.rm=TRUE)
        l=outcome.data.state[outcome.data.state[,outcome.p.col]==minimum,2] 
        l.good=l[!is.na(l)]
        return(as.character(l.good))
}

rankhospital<- function (state,outcome,rank) {
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
        l.2<-l.1[l.1$rankouk==rank,]
        l.2<-l.2[order(l.2$rankouk),]
        if(rank==min){
                min(l.1)
        }
return(as.character(l.2$Hospital.Name))       
}
