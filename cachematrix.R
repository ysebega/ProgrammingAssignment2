# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# The following two functions help in this context.

##makeCacheMatrix
## This function creates a special matrix object that caches its inverse
## makeCacheMatrix function does the following:
## sets the value of the matrix
## gets the value of the matrix
## set the value of the inverse of the matrix
## gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

## cacheSolve
## This function computes the inverse of the special matrix returned
## by makeCacheMatrix
## It fist checks if the inverse is computed. Otherwise the inverse is computed. 
## The assumption is that the matrix suplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinverse()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data)
  x$setinverse(invmat)
  invmat
}
