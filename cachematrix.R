## In this report, I write a pair of functions that cache the inverse of a matrix.
## we assume that the applied matrix is invertibal 
## HOW TO USE:
## Firstly we will create a invertible matrix x by using x <-  matrix()
## And then, we assign a variable a by using function of makeCacheMatrix 
##     a <- makeCacheMatrix(x)
## And then, call function of cacheSolve(a) that gives us the inverse matrix 
## of applied matrix x


##his function creates a special "matrix" object that can cache its inverse

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

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}