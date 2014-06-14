
# Matrix inversion is usually a costly computation and their may be
# some benefit to caching the inverse of a matrix rather than compute
# it repeatedly. This function creates a special matrix object that can
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Key assumption: the matrix supplied is always invertible.
  # The function is to:
  # 1. set the value of the matrix
  # 2. get the value of the matrix
  # 3. set the value of inverse of the matrix
  # 4. get the value of inverse of the matrix
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special matrix returned by
# makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

# Test run to see the results:
x <- as.matrix(cbind(c(2,2), c(3,2)))
test <- makeCacheMatrix(x)
test$get()
cacheSolve(test) # No caching.
cacheSolve(test) # Re-run. Get "getting cached data" message.

