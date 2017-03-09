## Caching the inverse of a matrix rather than compute it repeatedly is 
## beneficial. The makeCacheMatrix below creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  myInv <- NULL
  set <- function(y) {
    x <<- y
    myInv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) myInv <<- inverse
  getInverse <- function() myInv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the special "matrix" 
## created by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  myInv <- x$getInverse()
  if (!is.null(myInv)) {
    message("getting cached data")
    return(myInv)
  }
  matrx <- x$get()
  myInv <- solve(matrx, ...)
  x$setInverse(myInv)
  myInv
}
