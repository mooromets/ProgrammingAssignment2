## These functions manipulate a specific type of matrices
## that cache theirs inversion operation

## Creates a matrix with cached inversion operation

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function(newM) {
        x <<- newM
        mInv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) mInv <<- solve
    getsolve <- function() mInv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)    
}


## Returns an inversed matrix of 'x'

cacheSolve <- function(x, ...) {
    mInv <- x$getsolve()
    if(!is.null(mInv)) {
        message("getting cached data")
        return(mInv)
    }
    data <- x$get()
    mInv <- solve(data, ...)
    x$setsolve(mInv)
    mInv
}
