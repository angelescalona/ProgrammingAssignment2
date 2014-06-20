## These functions create a special "matrix" object that can cache its inverse.
## It is useful for big matrices where computing matrix inversion is 
## computationally expensive.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL
    set <- function(y){
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) sol <<- solve
    getsolve <- function() sol
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), the the cachesolve
## function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    sol <- x$getsolve()
    if(!is.null(sol)) {
        meassage("getting cached data")
        return(sol)
    }
    data <- x$get()
    sol <- solve(data, ...)
    x$setsolve(sol)
    sol
}
