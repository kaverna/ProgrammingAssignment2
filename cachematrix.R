## This files contains makeCacheMatrix and cacheSolve functions
## used to store and recever from a memory cache the inverse of a matrix

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

