# Instantiates a function to create a matrix
# and caches the matrix inverse
# matrix will have the following methods:
#     .set : sets the matrix
#     .get : gets the actual matrix
#     .setinvmat: set the inverse; this should not be called
#              by the user, but instead accessed through
#              the cacheSolve function below
#     .getinvmat: get the matrix inverse. NULL if
#              not already cached

makeCacheMatrix <- function(x = matrix()) {
# Inverse stored in invmat, actual matrix in x
    invmat <- NULL  # make sure the matrix is clean
  set <- function(y) {
    x <<- y  # inverse matrix to x
    invmat <<- NULL  # New matrix, so flush the old
  }
  get <- function() x  # Gets matrix
  setinvmat <- function(invmatx) invmat <<- invmatx
  getinvmat <- function() invmat
  list(set = set, 
       get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat) 
}


## Determines the inverse of the matrix x
## but uses the cached inverse result if available
##
##  x is the input matrix resulting from instantiating the makeCacheMatrix function
## ... optional arguments for the solve function
## the inverse matrix is returned
##

cacheSolve <- function(x, ...) {
  
  invmat <- x$getinvmat()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinvmat(invmat)
  return(invmat)
}
