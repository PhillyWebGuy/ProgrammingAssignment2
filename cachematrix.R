# This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(x) {
    m <<- x;
    inverse <<- NULL;
  }
  get <- function() return(m);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# This function computes the inverse of the matrix returned by makeCacheMatrix() 
# If the inverse has already been calculated, and the matrix has not changed, then
# cacheSolve() should obtain the inverse from the cache

cacheSolve <- function(m, ...) {
  inverse <- m$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- m$get()
  invserse <- solve(data, ...)
  m$setinv(inverse)
  inverse
}
