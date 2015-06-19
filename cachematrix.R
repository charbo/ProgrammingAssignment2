## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create an object which store a matrix and its inverse
## Before accessing to the inverse you have to run cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function takes an object of type  "makeCacheMatrix" as argument
## If the inverse have already been compute, the function return it
## Else the finction compute then return it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}