## The functions in this file calculate the inverse of a given matrix, cache
## both the original matrix and the result of the calculation and allow the
## recovery of the cached data.

## makeCacheMatrix creates a list of functions defined within its parent
## environment, which allow the storage of a given matrix and its inverse
## on cache for future use.

makeCacheMatrix <- function(x = matrix()) {
        ## 'i' will be used to store the inverse of the 'x' matrix      
        i <- NULL
        ## caches the matrix 'x' and resets 'i' to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## returns the cached matrix 'x'
        get <- function() x
        ## stores a matrix that is the inverse of 'x' on 'i'
        setinverse <- function(inverse) i <<- inverse
        ## returns the cached inverse of 'x'
        getinverse <- function() i
        
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## cacheSolve calculates and caches the inverse of the previously cached matrix.

cacheSolve <- function(x, ...) {
        ## Where 'x' is a list created with makeCacheMatrix(Matrix), and 'Matrix'
        ## is the original matrix, for which you want to calculate and cache its 
        ## inverse.
        i <- x$getinverse()
        ## Returns the cached inverse of 'Matrix', if available.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        ## Calculates the inverse of 'Matrix'
        i <- solve(data, ...)
        ## Stores the inverse of 'Matrix' in cache for future use
        x$setinverse(i)
        ## Returns a matrix 'i' that is the inverse of 'Matrix'
        i
}
