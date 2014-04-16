## The function makeCacheMatrix wraps a normal R matrix in an environment 
## that can cache its inverse. The function cacheSolve will return the 
## cached inverse from the cache if it has been calculated already, and 
## indicate that with a 'getting cached data' message.

## Example usage:

## > c
##      [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## > cm <- makeCacheMatrix(c)
## > cacheSolve(cm)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cacheSolve(cm)
## getting cached data
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4


## makeCacheMatrix takes a matrix and returns a cached matrix, which is really
## 4 functions to set and get the matrix and its inverse, as follows:
## - set: sets the cached matrix and resets the cached inverse to NULL
## - get: returns the cached matrix
## - setinverse: sets the cached inverse
## - getinverse: returns the cached inverse
##
## The matrix and its inverse are stored using <<- in the same environment as 
## those 4 functions, so are not visible directly to the user in the global 
## environment. However, they can be read in the proper environment as follows: 
## > get("cached_inverse", environment(cm$set))
##
## The cached inverse is initially set to NULL to indicate it hasn't been 
## calculated yet.

makeCacheMatrix <- function(cached_matrix = matrix()) {
    # default inverse is NULL
    cached_inverse <- NULL 
    # define the functions to get and set the matrix and its inverse
    set <- function(mat) {
        cached_matrix <<- mat
        cached_inverse <<- NULL
    }
    get <- function() cached_matrix
    setinverse <- function(inv) cached_inverse <<- inv
    getinverse <- function() cached_inverse
    # return the functions as result
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## cacheSolve takes a cached matrix as created by makeCacheMatrix and
## returns its inverse. 
##
## The function will return the cached inverse if it has been calculated 
## before and give a message 'getting cached data'. 
## If the inverse has not been calculated before, it will be calculated
## using the R function solve(), set in the cached matrix, and returned as 
## result of makeCacheMatrix. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    # return the cached inverse if it has been calculated before 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # otherwise, get the underlying regular R matrix
    mat <- x$get()
    # and calculate, set (cache) and return the inverse
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}
