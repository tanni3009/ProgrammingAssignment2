## The two functions below are used to create a special cache matrix 
## that stores a matrix and caches the inverse of the matrix.

## The function 'makeCacheMatrix' creates a special cache matrix
## which actually is a collection of functions to get/set the value
## of a matrix and get/set the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}



## The function 'cacheSolve' calculate the inverse of special cache matrix create with the function 'makeCacheMatrix'.
## First it checks if the inverse has already been calculated, if so, it returns the inverse from the cache,
## otherwise, it calculates the inverse, puts it in the cache and returns the inverse.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
