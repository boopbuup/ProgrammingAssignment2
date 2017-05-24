## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {   # set the matrix
    x <<- y
    inv <<- NULL
  }
  get = function() x   # get the matrix
  setinv = function(inverse) inv <<- inverse  # set the inverse
  getinv = function() inv # get the inverse
  list(set=set, get=get, setinv=setinv, getinv=getinv)   # list is used as the input to cacheSolve()
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
