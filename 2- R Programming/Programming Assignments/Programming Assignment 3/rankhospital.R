#part 2: Ranking hospitals by outcome in a state
source(file = "Helper.R")
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv(file = "outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    checkState(data, state)
    checkOutcome(outcome)
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    #computing column number based on "outcome" input
    colnumOutcome <- selectOutColNum(outcome)
    #making a data frame of specified state and with column of outcome, changing the outcome type to numeric and removing NAs
    stateHospitals <- stateOutcomeDF(data, state, colnumOutcome)
    #check that num is valid and set its value 
    newNum <- numSet(num, stateHospitals)
    rankes <- order(stateHospitals$outcome, stateHospitals$name)
    rankedHospitals <- stateHospitals[rankes, ]
    rankedHospitals[newNum, 1]
}

#test cases
# rankhospital("TX", "heart failure", 4)
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)