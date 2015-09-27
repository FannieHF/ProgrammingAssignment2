## Put comments here that give an overall description of what your
## functions do

## cachmatrix.R contains two functions, makeCacheMatrix and cacheSolve.
## With these two, one can cache the inverse of a matrix rather than compute
## it repeatedly.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse
    setinverse <- function(solve) s <<- solve
    # get the value of the inverse
    getinverse <- function() s
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## if there is no cached data or the matrix has changed
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
