## We want to ompute the inverse of a matrix while minimizing the memory usage of 
## the script by utilizing the cache. For this purpose, two functions have been designed:
##
## makeCacheMatrix()    creates a special matrix object so we can cache its inverse.
## cacheSolve()         computes the inverse and set it in the cache. If the inverse 
##                      was already computed, retrieves it from the cache.


## MAKECACHEMATRIX() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Arguments
    # @x	is an inversible matrix
    # 
    # Returns a list of functions that can be used as follow:
    #       x$set()     Set the matrix
    #       x$get()     Get the matrix
    #       x$setinv()  Set the inverse of the matrix
    #       x$getinv()  Get the inverse of the matrix
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## CACHESOLVE() takes an inversible matrix and returns its inverse

cacheSolve <- function(x, ...) {
    # Arguments
    # @x	is an inversible matrix
    # 
    # Returns the inverse of the original matrix x
    
    inv <- x$getinv()
    
    # If the inverse has already been cached, returns the object
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, get the matrix, compute its inverse and set it in the cache
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    
    # Return the inverse
    inv
}