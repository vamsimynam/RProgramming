rankall <- function(outcome,num="best"){
        
        outcomedf <- read.csv('outcome-of-care-measures.csv',colClasses = "character")
        outcomeList <- c("heart attack", "heart failure", "pneumonia")
        stateList <- sort(unique(outcomedf$State))
        hospitalList <- vector(mode='character')
        
        ## Validation of parameters
        if(!(outcome %in% outcomeList))
                stop("invalid outcome")
        
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
        outcomedf[, outcomeCol] <- as.numeric(outcomedf[, outcomeCol])
        ## Exclude NA from the dataframe
        outcomedf <- outcomedf[complete.cases(outcomedf[,outcomeCol]),]

        ## Sort the data by the outcome selected and then hospital name
        sortedOutcome <- outcomedf[order(outcomedf[,outcomeCol],outcomedf$"Hospital.Name"),]
        finalList <- transform(sortedOutcome,R=ave(sortedOutcome[,outcomeCol],sortedOutcome$State,FUN =function(x) rank(x,ties.method = "first")))
        
        ## Validation of rank
        for(i in 1:length(stateList)){
                final <- finalList[finalList$State==stateList[i],]
                hospital <- 
                (
                        if(num=='best') final[final$R==1,"Hospital.Name"]
                        else if (num=='worst') final[which.max(final$R),"Hospital.Name"]
                        else if(num <= which.max(final$R)) (final[which(final$R==num),"Hospital.Name"])
                        else 'NA'
                )
                #append the hospital list with the result
                hospitalList <- c(hospitalList,hospital)
        }
        # create a data frame with hospital & state names
        data.frame(hospital=hospitalList,state=stateList)
}
