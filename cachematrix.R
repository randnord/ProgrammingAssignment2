## The two functions below work together to cache the inverse of a matrix in
## order to prevent repeated computation

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve produces an inverse to the matrix from makeCacheMatrix.
## If matrix hasn't changed, the inverse will be retrieved from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m        ## Return a matrix that is the inverse of 'x'
}
