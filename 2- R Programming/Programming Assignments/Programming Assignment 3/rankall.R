#part 3: Ranking hospitals in all states
source(file = "Helper.R")
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv(file = "outcome-of-care-measures.csv", colClasses = "character")
    ## Check that outcome is valid
    checkOutcome(outcome)
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    States <- unique(data[, "State"])
    result <- data.frame(hospital = NA, state = States)
    #computing column number based on "outcome" input
    colnumOutcome <- selectOutColNum(outcome)
    #computing the result for each state
    for(i in result$state){
        stateHospitals <- stateOutcomeDF(data, i, colnumOutcome)
        newNum <- numSet(num, stateHospitals)
        rankes <- order(stateHospitals$outcome, stateHospitals$name)
        rankedHospitals <- stateHospitals[rankes, ]
        result[result$state == i, 1] <- rankedHospitals[newNum, 1]
    }
    #reordering result according to its state name
    result <- result[order(result$state), ]
}

#test cases
# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)
# tail(rankall("heart failure"), 10)