## Put comments here that give an overall description of what your
## functions do
##These functions take a matrix, finds the inverse and caches it and then get it from the cache.
## Write a short comment describing this function
##This function takes a matrix and finds the inverse and then caches it.
makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This function checks the cache to see if the inverse is there and if not, calculates it.
cacheSolve <- function(x, ...) {
   m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m      ## Return a matrix that is the inverse of 'x'
}
