## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly 

## This function creates a special "matrix" object
## that can cache its inverse.

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


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

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
