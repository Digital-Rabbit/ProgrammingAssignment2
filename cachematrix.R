## cachematrix.R
## Demonstrates how to cache data

## The makeCacheMatrix creates a special matrix object that can cache its inverse.
## It creates a list containing functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the matrix returned by the 
## makeCacheMatrix. If the inverse has been caclucated and the matrix has not 
## changed, then cacheSolve retireves the inverse from the cache.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
