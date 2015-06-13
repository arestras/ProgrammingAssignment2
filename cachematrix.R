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
    
    change <- function(y = matrix()) {                                      ## User can change the matrix x
        x <<- y
        inverse <<- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))       ## Resets to an empty matrix the inverse of x
    }
    
    getinverse <- function() {                                              ## Stores the inverse
        inverse
    }
    
    setinverse <- function(z = matrix()) {                                  ## User can input and store the inverse of x
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

    inverse <- x$getinverse()                                               ## Gets the inverse from makeCacheMatrix
    if(!anyNA(inverse)) {                                                   ## If inverse matrix is not empty,
        message("Getting cached data")                                      ## intimates the user about the cached inverse,
        return(inverse)                                                     ## and returns the cached inverse
    }
    inverse <- solve(x$get())                                               ## If inverse matrix has NA, inverse calculated
    x$setinverse(inverse)                                                   ## inverse stored in makeCacheMatrix 
    inverse                                                                 ## inverse of matrix x is printed
    
}
