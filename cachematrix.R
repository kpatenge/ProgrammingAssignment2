## This function creates a special "matrix" object that can cache its inverse.
## Functions are:
##   get  - to get a matrix
##   set  - to set a matrix
##   getInverse  - to get the inverse of a matrix
##   setInverse  - to set the inverse of a matrix

## makeCacheMatrix is a list of functions used to create a matrix and inverse of a matrix 

makeCacheMatrix <- function(m = matrix()) {
        inv_m <- NULL
        set <- function(n) {
                m <<- n
                inv_m <<- NULL
        }
        get <- function() m
        setInverse <- function(solve) inv_m <<- solve
        getInverse <- function() inv_m
        list(
			set = set, 
			get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## cacheSolve is a special function that verifies first whether the inverse of
## a matrix has already been calculated. If so it retrieves it from the cache.
## If not, it calculates the inverse of the matrix using function solve and sets 
## the inverse of the matrix in the cache using function setInverse.

cacheSolve <- function(m, ...) {
        inv_m <- m$getInverse()
		## Check if inverse has already been calculated. If so just get/return it.
        if (!is.null(inv_m)) {
            message("getting cached data")
            return(inv_m)
		}
        data <- m$get()
        inv_m <- solve(data, ...)
        m$setInverse(inv_m)
        inv_m
}
