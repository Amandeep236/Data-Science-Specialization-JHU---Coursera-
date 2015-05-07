#part 1: Finding the best hospital in a state
best <- function(state, outcome){
    ## Read outcome data
    data <- read.csv(file = "outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    checkState(data, state)
    checkOutcome(outcome)
    ## Return hospital name in that state with lowest 30-day death rate
    #computing column number based on "outcome" input
    colnumOutcome <- selectOutColNum(outcome)
    stateHospitals <- stateOutcomeDF(data, state, colnumOutcome)
    rankes <- order(stateHospitals$outcome, stateHospitals$state)
    rankedHospitals <- stateHospitals[rankes, ]
    rankedHospitals[1, 1]
}

#checking to see if the state is valid
checkState <- function(data, state){
    States <- unique(data[, "State"])
    if(!any(state == States)){
        stop("invalid state")
    }
}

#checking to see if the outcome is valid
checkOutcome <- function(outcome){
    Outcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!any(outcome == Outcomes)){
        stop("invalid outcome")
    }
}

#selecting and returning the column number based on outcome value
selectOutColNum <- function(outcome){
    colnumOutcome <- if(outcome == "heart attack"){
        11
    } else if(outcome == "heart failure"){
        17
    } else if(outcome == "pneumonia"){
        23
    }
    colnumOutcome
}

#making a data frame of specified state and with column of outcome, changing the outcome type to numeric and removing NAs
stateOutcomeDF <- function(data, state, colnumOutcome){
    stateHospitals <- data[data$State == state, c(2, colnumOutcome)]
    stateHospitals[, 2] <- as.numeric(stateHospitals[, 2])
    stateHospitals <- stateHospitals[complete.cases(stateHospitals), ]
    colnames(stateHospitals) <- c("state", "outcome")
    stateHospitals
}
#test cases
# best("TX", "heart attack")
# best("TX", "heart failure")
# best("MD", "heart attack")
# best("MD", "pneumonia")
# best("BB", "heart attack")
# best("NY", "hert attack")