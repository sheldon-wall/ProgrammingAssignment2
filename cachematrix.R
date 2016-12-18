## Week 3 - Peer-graded assignment - Lexical Scoping
## These functions calculate and cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.
##
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    
    ## ensure we are not attempting to find the inverse of a matrix 
    ## that has a determinant of zero - avoid a singular matrix
    if (det(data) != 0)
        inv <- solve(data, ...)
    else {
        inv <- NULL
        message("Warning: Inverse matrix is singular")
    }
        
    x$setinverse(inv)
    inv
}
##
## Function Tests
## 
## ## Test 1: Basic
## matx <- matrix(1:4, nrow = 2, ncol = 2)
## specialm <- makeCacheMatrix()
## specialm$set(matx)
## specialm$get()
## cacheSolve(specialm)
## ## The second time called should receive message getting cached data
## cacheSolve(specialm)
## ##
##
## Test 2: Basic - matrix changes
## matx <- matrix(1:4, nrow = 2, ncol = 2)
## specialm <- makeCacheMatrix()
## specialm$set(matx)
## specialm$get()
## cacheSolve(specialm)
## ## Change matrix after caching the previous inverse matrix
## matx <- matrix(c(2,4,8,1,3,7,3,6,9), nrow = 3, ncol = 3)
## specialm$set(matx)
## ## After changing the matrix we should not get a message about using cached data
## cacheSolve(specialm)
## ##
##
## ## Test 3; Advanced Test - trap for singular inverse matrix
## matx <- matrix(c(3,6,4,8), nrow=2, ncol=2)
## specialm <- makeCacheMatrix()
## specialm$set(matx)
## specialm$get()
## cacheSolve(specialm)
## ## 