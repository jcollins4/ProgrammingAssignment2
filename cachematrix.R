## This set of functions is used to 1.) set/get a matrix and set/get its inverse via lexical scoping 
## and then 2.) make use of 1. to cache matrices to reduce the need to recompute their inverses if 
## there has been no changes

## set/get a matrix and set/get its inverse via lexical scoping 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## return inverse of matrix if it has already been computed by calling cacheSolve
## if the index has not been computed, compute, and store in "special" matrix object
## with setinverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data:")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


