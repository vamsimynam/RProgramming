rankhospital <- function(state, outcome,rank){
        
        outcomedf <- read.csv('outcome-of-care-measures.csv',colClasses = "character")
        stateList <- unique(outcomedf$State)
        outcomeList <- c("heart attack", "heart failure", "pneumonia")
        
        ## Validation of parameters
        if(!(state %in% stateList))
                stop("invalid state")
        if(!(outcome %in% outcomeList))
                stop("invalid outcome")
        
        ## Filter the given state data
        stateOutcome <- outcomedf[outcomedf$State==state,]
        
        ## Select the column based on the outcome parameter value
        outcomeCol <- (
                if(outcome == "heart failure") { 
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                } else if (outcome == "heart attack") {
                        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                } else 
                        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        )
        ## Convert the "Not Available" to NAs
        stateOutcome[, outcomeCol] <- as.numeric(stateOutcome[, outcomeCol])
        ## Exclude NA from the dataframe
        stateOutcome <- stateOutcome[complete.cases(stateOutcome[,outcomeCol]),]
        #noofrows <- nrow(stateOutcomedf)
        
        ## Sort the data by the outcome selected and then hospital name
        sortedOutcome <- stateOutcome[order(stateOutcome[,outcomeCol],stateOutcome$"Hospital.Name"),]
        finalList <- transform(sortedOutcome,R=ave(sortedOutcome[,outcomeCol],FUN =function(x) rank(x,ties.method = "first")))
        
        ## Validation of rank
        if(rank=='best') return(finalList[which.min(finalList$R),"Hospital.Name"])
        else if (rank=='worst') return(finalList[which.max(finalList$R),"Hospital.Name"])
        else if(rank > which.max(finalList$R)) return('NA')
        else return(finalList[which.max(finalList$R==rank),"Hospital.Name"])
}
