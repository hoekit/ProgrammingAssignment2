################################################################################
##
## cachematrix.R
##
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. This script contains a pair of functions that cache the
## inverse of a matrix.
##
################################################################################

## makeCacheMatrix:  create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    # Returns cached matrix inverse
    cachedInverse <- function() inv

    # Returns plain matrix data
    matrixData <- function() x

    # Stores matrix inverse in cache
    storeInverse <- function(inverse) inv <<- inverse

    list( cachedInverse = cachedInverse,
          matrixData = matrixData,
          storeInverse = storeInverse )
}


## cacheSolve: computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cachesolve will retrieve the inverse from the cache.
## Assumption: matrix supplied is always invertible.

cacheSolve <- function(x, ...) {

    inv <- x$cachedInverse()    # Get the stored inverse from cache

    if (!is.null(inv)) {        # Return cached inverse if exists
        message("getting cached data")
        return(inv)
    }

    # Compute the matrix inverse
    inv <- solve(x$matrixData())

    # Store computed inverse into the cache
    x$storeInverse(inv)

    # Return computed inverse
    inv

}

