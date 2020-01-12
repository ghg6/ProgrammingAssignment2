## These functions calculates the inverse of a matrix, cache the value,
## and return the inverse if the inverse is cached

## Write a short comment describing this function
# Create special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solveinv) inv <<- solveinv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Cache matrix if inverse exists, solve for inverse if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
