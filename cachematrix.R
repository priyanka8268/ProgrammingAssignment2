## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## set the value of matrix 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of matrix
        get <- function() x
        ## set the value of Inverse of matrix
        setInverse <- function(inverse) inv <<- inverse
        ## get the value of Inverse of matrix
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## get value of inverse of matrix
         inv <- x$getInverse()
         ## check whether inverse is in inv variable
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        ## set value of inverse of matrix
        x$setInverse(inv)
        inv
}
