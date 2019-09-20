## Assignment 2: Lexical Scoping
## Caching the Inverse of a Matrix
## Since matrix inversion is a costly computation, we can write functions that catches the inverse of a matrix.
## The following functions serves this purpose by catching the inverse of a matrix.

## The function makeCacheMatrix can create a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The following function cacheSolve computes the inverse of the matrix returned by the function makeCacheMatrix above. 
## It will return the inverse from the cache if the inverse of the same matrix has already been calculated. 

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}