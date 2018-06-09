## The target of this code is making a function able of cache results from solve
## this way if the calculation has already been done it will be loaded from cache instead of calculated again

## This function makes an special object able to cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function first checks if the inverse of the matrix has been already calculated
## if the result has not benn calculated it is calculated and stored
## if the result has been already calculated then he stored value is returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
