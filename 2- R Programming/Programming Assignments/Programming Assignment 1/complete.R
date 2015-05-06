complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    #creating the output data frame
    result <- data.frame(id = id, nobs = rep(NA, length(id)))
    
    #adding result data for each id
    for(i in id){
        #obtaining the path for each file
        path <- makePath(directory, i)
        #reading the specified path
        data <- read.csv(file = path)
        #adding the number of complete cases of file to its position in result data frame
        result[which(id == i), 2] <- sum(complete.cases(data))
    }
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    result
}

#making the path for each file
makePath <- function(directory, i){
    path <- if(i < 10){
        paste0(directory, "/00", i, ".csv")
    } else if(i >= 10 && i < 100){
        paste0(directory, "/0", i, ".csv")
    } else{
        path <- paste0(directory, "/", i, ".csv")
    }
    path
}

#test cases
#complete("specdata", 1)
#complete("specdata", c(2, 4, 8, 10, 12))
#complete("specdata", 30:25)
#complete("specdata", 3)