## Cache the inverse of a matrix
## x should be an invertible matrix, no checks are made if not!
## For example, the following three commands indicate usage:
##
## my_matrix <- matrix(1:4, nrow = 2, ncol = 2) 
## my_test <- makeCacheMatrix(my_matrix)
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## assign to variables in different environment
        ## for caching purposes.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## Return a matrix that is the inverse of 'x'
## x should be the result of the invertible matrix being passed to makeCacheMatrix
## For example, the following three commands indicate usage:
##
## my_matrix <- matrix(1:4, nrow = 2, ncol = 2) 
## my_test <- makeCacheMatrix(my_matrix)
## cacheSolve(my_test)
## 
## This caches and returns the inverse of my_matrix
## Running cacheSolve(my_test) again returns the cached inverted matrix
## rather than performing a fresh calculation
##
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        ## Check if there is a cached value, if so return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If there is no cached value, invert the matrix, 
        ## cache it and return the results
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        return(m)
}