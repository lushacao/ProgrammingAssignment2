## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function take an argument x as the input matrix, object 
## mi to cache the inverse matrix. 
## The set() assigns the input argument to the x object in the parent 
## environment, and assigns the value of NULL to the mi object in the parent 
## environment. 
## The get() returns the input matrix.
## The setInverse() assigns the input argument to the value of mi in the parent 
## environment.
## The getInverse() returns the inverse matrix.
## The list() returns a fully formed object of type makeCacheMatrix() to be used 
## by downstream R code.

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) {
                mi <<- inverse
        }
        getInverse <- function() mi
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getInverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data)
        x$setInverse(mi)
        mi
}
