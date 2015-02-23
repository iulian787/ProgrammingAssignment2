makeCacheMatrix <- function(x = matrix()) {
#  makeCacheMatrix creates a special "matrix", which is 
# really an object with 2 member matrices and a list of four functions which
#
#   1)  set the value of the matrix to be inverted
#   2)  get the value of the matrix to be inverted
#   3)  set the value of the inverse
#   4)  get the value of the inverse
#
  inverse <- NULL
  set <- function(y) {
       x <<- y
       inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
# cacheSolve calculates the inverse of the special "matrix" created 
#  with the makeCacheMatrix function. 
#  It first checks to see if the inverse has already
#  been calculated. If so, it gets the inverse from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the 
# data and sets the value of the inverse in the cache via the 
#  setinverse function.
#
# Returns a matrix that is the inverse of input 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
