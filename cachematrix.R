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
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Like a social security check on the first of the month, it's getting cached...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
