#caches the matrix x

makeCacheMatrix <- function(x = matrix()) {
  
  inverseX <- NULL
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inverseX <<- solveMatrix
  getInverse <- function() inverseX
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#solves and returns inverse matrix of x

cacheSolve <- function(x, ...) {
        inverseX <- x$getInverse()
        if(!is.null(inverseX)) {
          message("getting cached data")
          return(inverseX)
        }
        data <- x$get()
        inverseX <- solve(data, ...)
        x$setInverse(inverseX)
        inverseX
        
}
