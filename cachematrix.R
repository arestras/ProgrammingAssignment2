## Following are a pair of functions to cache
## the inverse of an invertible matrix.

## The function makeCacheMatrix can
## store a matrix if provided
## change the matrix if provided
## cache the inverse if calculated
## set an inverse if provided.

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))            ## Creates an empty matrix for the inverse of x
    
    get <- function() {                                                     ## Returns the stored value for the matrix x
        x
    }
    
    change <- function(y = matrix()) {                                      ## Changes the values of matrix on user input
        x <<- y
        inverse <<- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
    }
    
    getinverse <- function() {                                              ## Stores the inverse
        inverse
    }
    
    setinverse <- function(z = matrix()) {                                  ## User input of inverse is possible for caching
        inverse <<- z
    }
    
    list(get = get, change = change, setinverse = setinverse, 
         getinverse = getinverse)
    
}

## The function cacheSolve takes the matrix from
## above and calculates its inverse using solve().
## If after the calculation, the above matrix
## is retained, then the cached inverse in the
## above funtion is returned.

cacheSolve <- function(x) {
    inverse <- x$getinverse()                                               ## Gets the inverse
    if(!anyNA(inverse)) {                                                   ## If inverse matrix does not have NA, returns
        message("Getting cached data")
        return(inverse)
    }
    inverse <- solve(x$get())                                               ## If inverse matrix has NA, inverse calculated
    x$setinverse(inverse)
    inverse
    
}
