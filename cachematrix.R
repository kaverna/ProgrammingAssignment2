## This files contains makeCacheMatrix and cacheSolve functions
## used to store and recover from a memory cache the inverse of a matrix


## makeCacheMatrix creates a function to hold the "invcache" and "x" variables
## contains get, set, get inv and setinv functions

makeCacheMatrix <- function(x) {
  invcache <- NULL
  set <- function(y) {
    x <<- y
    invcache <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invcache <<- inv
  getinv <- function() invcache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve looks for a value in x.getinv. If there's a value, returns the cached value. If there isn't calculates
## the inverse and stores in the function through setinv 

cacheSolve <- function(x) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    m
  }
  else
  {  
    message("calculating inverse")
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
  }
}