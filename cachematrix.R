## Matrix inverse is usually a costly computation. Hence, there may be some benefit
## from caching the value of the matrix inverse rather than computing it repeatedly.

## This first function creates a special "matrix" object that allows us to cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinv <- function(solve) n <<- solve
  getinv <- function() n
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## This second function computes the inverse of the special "matrix" created by
## the function makeCacheMatrix. If the inverse has been calculated before and the
## matrix has not changed, the function cacheSolve would retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinv()
        if(!is.null(n)) {
          message("getting cached data")
          return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinv(n)
        n
}
