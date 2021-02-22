## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Creates a object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: returns the inverse of a matrix cached in an object created with the funtion makeCacheMatrix. 
## If the inverse of the matrix has not being calculated yet, it is calculated using the function silve(x)
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached inverted matrix")
    return(inv)
  }
  data <- x$get()
  inv <- mean(data, ...)
  x$setsolve(inv)
  inv
}
