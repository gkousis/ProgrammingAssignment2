## GK: R Lexical Scoping Assignment 2
## Exploit R's lexical scoping to introduce caching for expensive operations.
## These functions create a special matrix object that contains the matrix as well
## as the cached inverse of the matrix and functions that allow the user to set or get the matrix


## Function makeCacheMatrix
## returns a list object that contains 4 functions that to get/set the matrix and to set/get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize the cachedInverse to NULL.  This way the helper function knows the inverse is not already calculated
  cachedInverse <- NULL
  
  ## when the user changes the source matrix using the set function, reset the cached inverse to NULL
  set <- function(y) {
    ## Assign the value at the parent environment
    x <<- y
    cachedInverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  getInverse <- function() cachedInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function cacheSolve
## Calculates the inverse of the matrix in the special matrix object and returns the cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ci <- x$getInverse()
  if(!is.null(ci)) {
    message("getting cached data")
    return(ci)
  }
  
  sourceMatrix <- x$get()
  ci <- solve(sourceMatrix, ...)
  x$setInverse(ci)
  ci
}
