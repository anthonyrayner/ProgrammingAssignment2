## Matrix inversion is sually a costly computation, and there may be some 
## benefit to caching the inverse of a matrix rather than compte it repeatedly
## These functions cache the inverse of a matrix

## 
## 

## This function creates a special "matrix" object that can cache its inverse

## It will:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the invoice

makeCacheMatrix <- function(x = matrix()) {
    
    theInverse <- NULL
    
    set <- function(y) {
        x <<- y
        theInverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) theInverse <<- inverse
    getInverse <- function() theInverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
    
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.  If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    theInverse <- x$getInverse()
    
    if(!is.null(theInverse)) {
        message("Getting cached data")
        return(theInverse)
    }
    
    data <- x$get()
    theInverse <- solve(data, ...)
    x$setInverse(theInverse)
    theInverse
}