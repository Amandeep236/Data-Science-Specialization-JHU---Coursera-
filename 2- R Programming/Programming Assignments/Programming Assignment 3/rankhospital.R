#part 2: Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv(file = "outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    checkState(data, state)
    checkOutcome(outcome)
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    colnumOutcome <- selectOutColNum(outcome)
    stateHospitals <- stateOutcomeDF(data, state, colnumOutcome)
    #check that num is valid and set its value 
    newNum <- numSet(num, stateHospitals)
    rankes <- order(stateHospitals$outcome, stateHospitals$name)
    rankedHospitals <- stateHospitals[rankes, ]
    rankedHospitals[newNum, 1]
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

#checking to see if num is not numeric, setting num for "best" & "worst" and otherwise stoping with a message
numSet <- function(num, stateHospitals){
    if(class(num) != "numeric" && class(num) != "integer"){
        num <- if(num == "best"){
            1
        } else if(num == "worst"){
            nrow(stateHospitals)
        } else {
            stop("invalid num")
        }
    }
    num
}

#making a data frame of specified state and with column of outcome, changing the outcome type to numeric and removing NAs
stateOutcomeDF <- function(data, state, colnumOutcome){
    stateHospitals <- data[data$State == state, c(2, colnumOutcome)]
    stateHospitals[, 2] <- as.numeric(stateHospitals[, 2])
    stateHospitals <- stateHospitals[complete.cases(stateHospitals), ]
    colnames(stateHospitals) <- c("name", "outcome")
    stateHospitals
}

#test cases
# rankhospital("TX", "heart failure", 4)
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)