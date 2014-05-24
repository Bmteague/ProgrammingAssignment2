## Function 1: makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## Function 2: cacheSolve computes the inverse of the "special" matrix returned by 
##     makeCacheMatrix above. 
##     If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
##     will retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list( set = set, get = get, setInverse = setInverse,
        getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if ( ! is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-inverse(data, ...)
  x$setInverse(m)
  m
}
