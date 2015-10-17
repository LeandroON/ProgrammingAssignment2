## The functions in this file calculate the inverse of a given matrix, cache
## both the original matrix and the result of the calculation and allow the
## recovery of the cached data.

## makeCacheMatrix creates a list of functions defined within its parent
## environment, which allow the storage of a given matrix and its inverse
## on cache for future use.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## 'i' will be used to store the inverse of the 'x' matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## caches the matrix 'x' and resets 'i' to NULL
        get <- function() x
        ## returns the cached matrix 'x'
        setinverse <- function(inverse) i <<- inverse
        ## stores a matrix that is the inverse of 'x' on 'i'
        getinverse <- function() i
        ## returns the cached inverse of 'x'
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
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        ## Returns the cached inverse of 'Matrix', if available.
        }
        data <- x$get()
        i <- solve(data, ...)
        ## Calculates the inverse of 'Matrix'
        x$setinverse(i)
        ## Stores the inverse of 'Matrix' in cache for future use
        i
        ## Returns a matrix 'i' that is the inverse of 'Matrix'
}