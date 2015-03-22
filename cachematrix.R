## This file contains functions that cache the inverse of a matrix
## so that the inverse can be looked up rather than recomputed.

## This function creates a data structure that contains a 
## matrix, and its inverse and accessors
## for both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function returns the inverse of a matrix in 
## conjunction with a cacheMatrix data structure.
## It creates the cached inverse if it does not exist, or
## returns the contents of the cache if it has already 
## been computed.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

