## 
## Cache costly matrix operations.
## Only matrix inversion cached at the moment.
##
## Usage: (m is an invertible matrix)
##    mc <- makeCacheMatrix(m)
##    minv <- cacheSolve(mc)
##    ...
##    minv2 <- cacheSolve(mc)
##    

## Matrix cache

makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) matInverse <<- inverseMatrix
  getInverse <- function() matInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of a matrix. Returns cached if available, otherwise
## solves, caches and return the inverse.

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
