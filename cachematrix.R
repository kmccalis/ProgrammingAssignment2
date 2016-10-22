## makeCacheMatrix will create a list containg (4) callable functions. 
## set() which can be called from the list using x$set() will set the value y
## equal to x in the parent environment and Null to inv in the parent environment
## get() retrieves the value of x from the parent environment of makeCacheMatrix
## setInv() inv <<- solve will assign solve to inv in the parent environment
## getInv() inv retrieves the value of inv from the parent environment 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## compute the inverse of the matrix from makeCacheMatrix(). If the matrix
## has not changed (already been solved) the function will retrieve the value
## from the cache. If it has changed "data" will get the matrix and the inverse 
## will be calculated and stored in "inv".

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
      message("Getting Cached Data")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
  }
