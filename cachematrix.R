## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not cover here).

## We assume that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
##
## The first function, makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to:
## - set the matrix
## - get the matrix
## - set the matrix inverse
## - get the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    # Reset inverse if matrix is set, so we don't have a wrong cached value
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) matrixInverse <<- solve
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  matrixInverse <- x$getInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- x$get()
  matrixInverse <- solve(data, ...)
  x$setInverse(matrixInverse)
  matrixInverse
}
