## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will create an abstraction around a matrix that
## will allow for the defaults of the cacheInverse to be cached
## 
makeCacheMatrix <- function(x = matrix()) {
    cacheValue <- NULL
    set <- function(y) {
      x <<- y
      cacheValue <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cacheValue <<- inverse
    getinverse <- function() cacheValue
    list(set = set, get = get,
         getinverse = getinverse,
         setinverse = setinverse)
}


## cacheSolve will get the inverse matrix for the provided matrix
## this will cache the value so that it is only calculated once
## per call to x$set. This matrix should be made via the makeCacheMatrix
## function to ensure that the matrix has the required cache functions
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  inverseValue <- solve(data)
  x$setinverse(inverseValue)
  inverseValue
}
