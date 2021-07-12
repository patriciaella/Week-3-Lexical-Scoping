## These functions are contributed as part of the R Programming course on Coursera.

## This function generates a special "matrix" object capable of caching its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The inverse of the special "matrix" returned by makeCacheMatrix above is computed by this function. 
## If the inverse has already been determined (and the matrix has not changed),
## The cachesolve function should then get the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
