## Put comments here that give an overall description of what your
## functions do
## Functions to cache the inverse of a matrix
## If the matrix has not changed and the inverse has been calculated, 
## then the cacheSolve will retrieve the inverse from the cache

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  set <- function(y) {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes of the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if ((!is.null(m)) & (x&get() == x)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
