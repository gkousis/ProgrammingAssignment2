## GK: R Lexical Scoping Assignment 2
## Exploit R's lexical scoping to introduce caching for expensive operations.
## These functions create a special matrix object that contains the matrix 
## as well as the cached inverse of the matrix and functions that allow 
## the user to set or get the matrix


## Function makeCacheMatrix
## returns a list object that contains 4 functions that to get/set the matrix
## and to set/get the inverse. 
## function takes optional parameter the matrix object.  
## If not provided, an empty matrix is created
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize the cachedInverse to NULL.
    cachedInverse <- NULL
    
    ## sets the matrix value
    set <- function(y) {
        ## Assign the value at the parent environment
        x <<- y
        ## when the user changes the matrix reset the cached inverse to NULL    
        cachedInverse <<- NULL
    }
    
    ## return the matrix
    get <- function() x
    
    ## set the inverse of the matrix.  This is provided by the caller.
    ## This design is as per assignment guidelines but can lead to mistakes
    ## if the caller does not pass the true inverse of the matrix.
    ## A better approach would have been to combine the cacheSolve function with 
    ## the getInverse function
    ## Note that the cachedInverse is set at the parent environment
    setInverse <- function(inverse) cachedInverse <<- inverse  
    
    ## simply return the cachedInverse (used by the cacheSolve function)
    getInverse <- function() cachedInverse
    
    # retun a list of the 4 functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Function cacheSolve
## Checks if a cached inverse of the matrix exists and returns it.
## Otherwuise, it Calculates the inverse of the matrix in the special matrix object, 
## caches it and and returns the result

cacheSolve <- function(x, ...) {
    ## Get the existing cached inverse
    ci <- x$getInverse()
    ## check if not null, simply return the result
    if(!is.null(ci)) {
        message("getting cached data")
        return(ci)
    }
    
    ## if the cached value was null...
    sourceMatrix <- x$get()
    ## calculate the inverse
    ci <- solve(sourceMatrix, ...)
    ## store it
    x$setInverse(ci)
    ## return
    ci
}
