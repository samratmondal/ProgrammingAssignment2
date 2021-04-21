## This file has two functions that implements caching of inverse 
## matrix value of an invertible matrix for reuse

## This function caches the inverse value of the matrix in higher env.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    message("setinverse called")
    m <<- inverse
  }
  
  getinverse <- function() {
    message("getinverse called")
    m
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function inverses an invertible matrix and caches the result 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("NOT getting cached data")
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
