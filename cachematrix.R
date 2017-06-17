## These functions will create and store a matrix with the following
## properties, set, get, setInverse, getInverse. 
##
## set will create the matrix to be stored, by either passing a matrix
## through as an argument, or the matrix definition call as an argument.
##
## test$set(matrix(c(1,2,3,4, 11,12,13,14), nrow = 2, ncol = 4, byrow = TRUE))
## test$set(mat.example)

## Once the matrix has been set, the inverse can be calculated and cached with the 
## cacheSolve function.

## Calling cacheSolve on the special matrix we have created, will check to make sure
## the matrix can have an inverse (it is square), and the compute the inverse and store it
## for later use, without having to recalculate it every time.

## Write a short comment describing this function
## makeCacheMatrix: Creates a matrix that has several functions attached to it

makeCacheMatrix <- function(x = matrix()) {
  s = NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv) s <<- inv
  getInverse <- function() s
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve: Tests if a matrix can have an inverse, and then calculates and stores the inverse
## of that matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  else if(nrow(x$get()) != ncol(x$get())) {
    message("Non-Square Matrix, cannot compute Inverse")
    return()
  }
  
  data <- x$get()
  s <- solve(data)
  x$setInverse(s)
  s
}
