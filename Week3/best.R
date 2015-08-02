# The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# 
#  If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals \b", \c",
# and \f" are tied for best, then hospital \b" should be returned).

best <- function(state, outcome){
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
        
        ## Sort the data by the outcome selected and then hospital name
        sortedOutcome <- stateOutcome[order(stateOutcome[,outcomeCol],stateOutcome$"Hospital.Name", na.last = NA),]
        
        ## Find the minimum value of the selected outcome and return the hospital name
        sortedOutcome[which.min(sortedOutcome[,outcomeCol]),"Hospital.Name"]
}
