pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    #creating a vector to store specified pollutant information for all specified ids
    resultVector <- numeric()
    
    #adding pollutant information from each specified file to the result vector
    for(i in id){
        #obtaining the path for each file
        path <- makePath(directory, i)
        #reading the specified path
        data <- read.csv(file = path)
        #making a logical vector to identify NA pollutant values
        bad <- is.na(data[, pollutant])
        #appending good pollutant values to reslut vector
        resultVector <- c(resultVector, data[!bad, pollutant])
    }
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    mean(resultVector)
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
#pollutantmean("specdata", "sulfate", 1:10)
#pollutantmean("specdata", "nitrate", 70:72)
#pollutantmean("specdata", "nitrate", 23)