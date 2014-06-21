## With interconnected two functions described below, we can save time by 
## getting the result which was already calculrated and cached instead of
## redoing it.


## makeCacheMatrix is a function whereby you can set a new matrix,  
## and the inverse matrix of it will be chached.

makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) Inverse <<- solve
        getInverse <- function() Inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function calculates the inverse of the matrix created with
## the makeCacheMatrix function. This function will first check whether 
## the inverse matrix is alreay created. If so, it will get it from 
## the cache.


cacheSolve <- function(x, ...) {
        Inverse <- x$getInverse()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setInverse(Inverse)
        Inverse
}