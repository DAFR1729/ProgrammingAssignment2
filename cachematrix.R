## These functions compute the inverse of a matrix, and cache its value
## for future retrieval

## Comment for makeCacheMatrix
## Creates a list containing a matrix and functions to store and retrieve
## its value and the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
        # The variable m contains the inverse of the matrix,
        # initialize it to NULL
        m <- NULL
        
        # Function to set the value of the matrix x, reset the inverse
        # of the previous value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Function to retrieve value of matrix
        get <- function() x
        
        # Function to set the cached value of inverse
        setsolve <- function(solve) m <<- solve
        
        # Function to retrieve the cached value of inverse
        getsolve <- function() m
        
        # Return the list with all of the elements
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Comment for cacheSolve
## This function retrieves the cached value of the inverse of a matrix
## if it exists; otherwise, it computes it and caches it for future use

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        # First, try getting a cached value
        m <- x$getsolve()
        
        # If cached value is not NULL, retrieve and return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Otherwise, get value of matrix, then invert it, cache it and return in
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
