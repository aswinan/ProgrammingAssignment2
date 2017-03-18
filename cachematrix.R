## Put comments here that give an overall description of what your
## functions do
## The pair of functions below can be used to create a special matrix and caches its inverse rather than computing the inverse repeatedly
## as inverson calculation can be a costly computation


## Write a short comment describing this function
## This function creates a special matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix created by the special function above
## If the inverse is already calculated, then the function should retrieve the information from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}






