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
    rankes <- order(stateHospitals$outcome, stateHospitals$name)
    rankedHospitals <- stateHospitals[rankes, ]
    rankedHospitals[1, 1]
}

#test cases
# best("TX", "heart attack")
# best("TX", "heart failure")
# best("MD", "heart attack")
# best("MD", "pneumonia")
# best("BB", "heart attack")
# best("NY", "hert attack")