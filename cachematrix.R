## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. There are two functions below to create a special
## "matrix" object and compute the inverse of its special object.


## The function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse. This special "matrix" object stores inside 
## original matrix and its inverse value in the cache.
makeCacheMatrix <- function(x = matrix()) {
    ## reset cache value
    m <- NULL
    ## define function that stores original matrix and resets the cache value
    set <- function(y) {
        ## check if matrix was changed, if true - store new matrix and reset
        ## the cache, overwise the stored matrix and cached value stay without
        ## changes.
        if (!identical(x, y)) {
            x <<- y
            m <<- NULL
        }
    }
    ## define function that returns the original matrix
    get <- function() x
    ## define function that stores inverse value of the matrix x
    setinv <- function(inv) m <<- inv
    ## define function that returns inverse value of the matrix x
    getinv <- function() m
    ## return list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## try to get cached data
    m <- x$getinv()
    ## check if there is some stored data in the cache
    if(!is.null(m)) {
        ## display message
        message("getting cached data")
        ## return cached value from funciton
        return(m) 
    }
    ## get x data self
    data <- x$get() 
    ## calculate inverse matrix
    m <- solve(data, ...) 
    ## store inverse matrix into the cache
    x$setinv(m) 
    ## return inverse matrix
    m
}
