## functions to create matrixes enabled to cache results of operations

## makeCacheMatrix creates a matrix (enabled to cache results of the inverse-operations)

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mi <<- inverse
  getinverse <- function() mi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of matrix (using the cached inverse if available)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setinverse(mi)
  mi
}
