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


## The function cacheSolve() first attempts to retrieve a inverse matrix from the 
## object passed in as the argument.if the value is not equal to NULL, the 
## function returns a valid, cached inverse matrix to the parent environment.
## If the result of !is.null(m) is FALSE, cacheSolve() gets the matrix from the 
## input object, calculates a solve(), uses the setInverse() function on the 
## input object to set the inverse matrix in the input object, and then returns 
## the value of the inverse matrix to the parent environment by printing the 
## inverse matrix object.

cacheSolve <- function(x, ...) {
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
