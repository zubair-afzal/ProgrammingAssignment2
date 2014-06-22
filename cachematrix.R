## Functions in this file are used to cache the inverse of a Matrix

## makeCacheMatrix function creates a list with four functions
##	set -> to set the Matrix
##  get -> to get the Matrix
##  setinv -> to set the inverse of the Matrix
##  getinv -> to get the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y){
  	## cache Matrix in mat
    mat <<- y
    ## since it is a new Matrix and no inverse has been calculate for it so far
    ## set inv variable which store the inverse of a Matrix to NULL
    inv <<- NULL
  } 

  ## Return the Matrix which was set using the set function
  get <- function() mat
  
  ## cache inverse of the Matrix in the inv variable
  setinv <- function(solve) inv <<- solve
  
  ## return the inverse matrix
  getinv <- function() inv
  
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function checks if the inverse of the Matrix has already been calculated and stored in the environment, it will
## return the inverse if found otherwise it will calculate the inverse using the R solve function and store it

cacheSolve <- function(x, ...) {
  ## get the inverse of the Matrix from cache
  inv <- x$getinv()

  ## check if the inv variable got any value
  if(!is.null(inv)){
  	## inv got the inverse Matrix
    message("getting cached matrix data")
    ## return the inverse Matrix
    return(inv)
  }

  ## there was no inverse cached
  message("calculating and setting matrix inverse")

  ## get the Matrix for which the inverse is to be calculated
  data <- x$get()

  ## calculate the inverse using solve function
  inv <- solve(data, ...)

  ## store the inverse in the cache
  x$setinv(inv)

  ## return the inverse Matrix
  inv 
}