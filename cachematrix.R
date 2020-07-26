## Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it
##repeatedly 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmat <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmat <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(setmat = setmat,
         getmat = getmat,
         setmean = setinv,
         getmean = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <-x$getinv()
    if(!is.null(inv)){
        message("Getting Chached Data")
        return(inv)
    }
    mat <- x$getmat()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
}
