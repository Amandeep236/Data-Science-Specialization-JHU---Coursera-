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
    #removing redundant columns
    dataComp <- data[, c(2, 7, colnumOutcome)]
    #removing rows with NAs
    dataComp <- dataComp[complete.cases(dataComp), ]
    #sorting based on states
    dataSort <- sort(dataComp[, 2])
    #spliting data based on state using tapply, sorting with outcome and name, returning name with rank "num"
    tapply(dataSort, dataSort[, 2], FUN = function{
        res <- dataComp[sort(dataComp[, 3], dataComp[ ,1]), ]
        if(num == "best"){           
            res[1, 1]
        } else if(num == "worst"){
            res[nrow(res), 1]
        } else if(class(num) == "numeric" || class(num) == "integer"){
            res[num, 1]
        }
    })
}

#test cases
# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)
# tail(rankall("heart failure"), 10)