## This module transforms a matrix structure into an extended 
## structure which includes the matrix itself and (eventually)
## a cached copy of its inverse (we assume that this structure
## is always used for invertible matrices); the first request 
## for the inverse is computed, then the outcome is saved, and 
## afterwards the same outcome is produced in request. If a new
## assignment is made for the same matrix variable, then the 
## inverse is set to NULL and the cycle repeats.

## setting up the matrix for caching

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() { x }
        setinverse <- function(thissolve) { m <<- thissolve }
        getinverse <- function() { m }
        list(set = set, 
			 get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## computing, caching, and presenting the inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}