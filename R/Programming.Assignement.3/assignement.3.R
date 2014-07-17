best<- function (state,outcome) {
        outcome.data<- read.table("~/Github/datasciencecoursera/outcome-of-care-measures.csv",
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
        if (outcome.p.col!=11||17||23) {
                stop("invalid outcome")
        }  
        minimum=min(
                min.outcome<-outcome.data.state[,outcome.p.col],
                na.rm=TRUE)
        l=outcome.data.state[outcome.data.state[,outcome.p.col]==minimum,2] 
        l.good=l[!is.na(l)]
        return(as.character(l.good))
}