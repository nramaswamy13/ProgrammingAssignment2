# Matrix inversion is usually a costly computation.There is some benefit
# to caching inverse of a matrix rather than compute it over and over again. Two 
# functions listed below are used to cache the inverse of a matrix.

# makeCacheMatrix function creates a list containing a function to
# a. Set value of matrix
# b. Get value of matrix
# c. Set value of inverse matrix
# d. Get value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }
}

# The function listed below returns inverse of the matrix. It first checks to see if
# the inverse has already been computed. If it has been computed, then it gets the result and skips the
# computation. If it hasnot been computed, then it computes the inverse, and then sets the value in the cache via
# the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
