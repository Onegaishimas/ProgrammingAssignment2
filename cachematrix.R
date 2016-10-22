## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinvmat <- function(invmatx) invmat <<- invmatx
  getinvmat <- function() invmat
  list(set = set, 
       get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat) 
}


## Write a short comment describing this function

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
