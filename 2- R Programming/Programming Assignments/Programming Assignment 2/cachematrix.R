#"makeCacheMatrix" is a function that creates a special kind of object that stores a matrix and cache's its inverse.
#this function has a variable "im" for storing the inverse of the matrix and 4 function for setting and getting the
#matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
    #creating an inverse matrix named "im"
    im <- NULL
    #creating set function for setting the matrix and setting it's inverse to NULL (because inverse matrix for this
    #new matrix has not been computed yet)
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    #creating get function for obtaining the matrix
    get <- function() x
    #creating a function for setting the inverse of the matrix in cache
    setinverse <- function(inverseM) im <<- inverseM
    #creating a function for getting the inverse of the matrix from cache
    getinverse <- function() im
    #returning functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The following function calculates the inverse of the special "matrix" created with the above function. However, it
# first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips
# the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache
# via the setinverse function.
cacheSolve <- function(x, ...) {    
    #getting the cached inverse of the matrix
    im <- x$getinverse()
    #checking to see if the inverse is cached. if the inverse is cached, print a message and return the cached inverse.
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    #if inverse is not cached, we should get the matrix (and store it in matrix named data) & compute its inverse with
    #"solve" function, store the inverse in cache (via "setinverse" function) and return the inverse matrix.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(im)
    ## Return a matrix that is the inverse of 'x'
    im
}