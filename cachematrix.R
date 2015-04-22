## These functions can be used to store matrix and it's inverse.
## makeCacheMatrix is used to create a matrix capable to cache it's inverse.
## cacheSolve is called to get inverse of a matrix created with makeCacheMatrix.

## This function creates a cached matrix which can store it's inverse.
## Stored inverse can be retrieved without the need to compute it again.
## Argument x - An invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
    ## Cache for computed inverse.
    inverse_matrix <- NULL
    
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) {
        inverse_matrix <<- inverse
    }
    
    getinverse <- function() inverse_matrix
    
    ## Return list of functions.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns inverse of a matrix.
## If inverse for the matrix has been previously computed and cached,
## the cached inverse is returned.
## Argument x - Matrix created with makeCacheMatrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getinverse()
        
    if(!is.null(inverse)) {
        ## Cached inverse exists.
        message("getting cached inverse")
        return(inverse)
    }
    
    ## Calculate inverse and cache it.
    message("calculating new inverse")
    m <- x$get()
    inverse <- solve(m, ...)
    x$setinverse(inverse)
    
    ## Return the inverse of matrix x.
    inverse
}
