## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
##Setting the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
##Getting the matrix
  get <- function() x
##Seting the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
##Getting the inverse of the matrix
  getInverse <- function() inv
## Return the list of the methods
   list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
## Return the inverse 
   if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
## Get the matrix.
  mat <- x$get()
  inv <- solve(mat, ...)
## Set the inverse to the object
  x$setInverse(inv)
  inv
}
}
