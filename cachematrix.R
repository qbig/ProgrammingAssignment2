## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # val for the inverse value for the matrix in the scope
  inv <- NULL
  
  # the setter function for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # the getter function for the matrix
  get <- function()
    x
  
  
  # setter function for the inverse value
  setinv <- function(val)
    inv <<- val
  
  # getter function for the inverse value 
  getinv <- function()
    inv
  
  # return the list
  list(
    set = set, get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # if has cache, return
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if no cache, compute the inverse value, cache it, return
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
