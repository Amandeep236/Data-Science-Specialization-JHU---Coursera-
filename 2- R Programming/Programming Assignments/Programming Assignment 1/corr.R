corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    #creating the output correlation vector
    corVec <- numeric()
    #reading file names from the directory and making the ids for "id" argument of "complete" function
    #this computation can be removed from this specific example (complete(directory) will work), but this is a more
    #comprehensive way of finding the file names!
    fileNames <- dir(path = directory)
    fileIDs <- as.integer(substr(x = fileNames, start = 1, stop = 3))
    #getting the number of complete cases for each file and making a data frame of it
    completeDF <- complete(directory = directory, id = fileIDs)
    #seubsetting above threshold cases and making above threshold data frame
    aboveTDF <- completeDF[completeDF$nobs > threshold, ]
    #removing undesired row names from aboveTDF
    row.names(aboveTDF) <- NULL
    #checking if there is any file with above the threshold value for complete cases
    if(nrow(aboveTDF) > 0){
        #computing correlation for each cases in aboveTDF
        for(i in 1:nrow(aboveTDF)){
            path <- makePath(directory = directory, i = aboveTDF[i, 1])
            data <- read.csv(file = path)
            good <- complete.cases(data)
            corVec[i] <- cor(x = data[good, 2], y = data[good, 3])
        }
    }       
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    corVec
}

#test cases
# cr <- corr("specdata", 150)
# head(cr)
# summary(cr)
# cr <- corr("specdata", 400)
# head(cr)
# summary(cr)
# cr <- corr("specdata", 5000)
# summary(cr)
# length(cr)
# cr <- corr("specdata")
# summary(cr)
# length(cr)