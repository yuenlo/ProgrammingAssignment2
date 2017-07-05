## makeCacheMatrix : This function creates a special "matrix" object that can
##   cache its inverse.
## cacheSolve : This function computes the inverse of the special "matrix"
##   returned by  makeCacheMatrix  above. If the inverse has already been 
##   calculated (and the matrix has not changed), then cacheSolve should 
##   retrieve the inverse from the cache.

## makeCacheMatrix returns a list of functions:
## set: sets the value of the cached matrix, and invalidates the cached inverse
##      matrix
## get: get the cached matrix
## setSolve: sets the value of the cached inverse matrix
## getSolve: gets the value of the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function (solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve returns a matrix that is the inverse of 'x' 
##   Returns it from cache if it's been calculated
##   Calculates it if it's not cached and saves to cache
cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}