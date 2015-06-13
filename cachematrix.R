## Following are a pair of functions to cache
## the inverse of an invertible matrix.

## The function makeCacheMatrix can
## store a matrix if provided
## change the matrix if provided
## cache the inverse if calculated
## set an inverse if provided.

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
    
    get <- function() {
        x
    }
    
    change <- function(y = matrix()) {
        x <<- y
        inverse <<- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
        
    }
    
    getinverse <- function() {
        inverse
    }
    
    setinverse <- function(z = matrix()) {
        inverse <<- z
    }
    
    list(get = get, change = change, setinverse = setinverse, getinverse = getinverse)
    
}

## The function cacheSolve takes the matrix from
## above and calculates its inverse using solve().
## If after the calculation, the above matrix
## is retained, then the cached inverse in the
## above funtion is returned.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!anyNA(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    inverse <- solve(x$get())
    x$setinverse(inverse)
    inverse
    
}
