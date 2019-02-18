## Matrix inversion is usually a costly computation 
##and there may be some benefit to caching the inverse of a matrix 
##rather than computing it repeatedly. In this example we introduce
##the <<- operator which can be used to assign a value to an 
##object in an environment that is different from the current 
##environment.

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

##Computing the inverse of a square matrix can be done with 
##the solve function in R. For example, if X is a square 
##invertible matrix, then solve(X) returns its inverse.

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

x =rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()
