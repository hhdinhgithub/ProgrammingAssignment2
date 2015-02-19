# Assigment 2 Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
#
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# the matrix must be invertible, else the function will show error

cacheSolve <- function(mtx, ...) {
    inv <- mtx$getinv()
    if(!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    data <- mtx$get()
    inv <- solve(data)
    mtx$setinv(inv)
    inv
}

## Sample run and test
## source("cachematrix.R") 
## > x <- rbind(c(1, 3), c(2, 4))
## > mtx <- makeCacheMatrix(x)
## > mtx$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## No cache in the first run
## > cacheSolve(mtx)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Retrieving from the cache from second run onward
## > cacheSolve(mtx)
## Getting cached data...
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
