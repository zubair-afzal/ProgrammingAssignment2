## Functions in this file are used to cache the inverse of a Matrix

## makeCacheMatrix function creates a list with four functions
##	set -> to set the Matrix
##  get -> to get the Matrix
##  setinv -> to set the inverse of the Matrix
##  getinv -> to get the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y){
    mat <<- y
    inv <<- NULL
  } 
  get <- function() mat
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function checks if the inverse of the Matrix has already been calculated and stored in the environment, it will
## return the inverse if found otherwise it will calculate the inverse using the R solve function and store it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached matrix data")
    return(inv)
  }
  message("calculating and setting matrix inverse")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv 
}