## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                   ## defining inv as empty matrix
  set <- function(y){           ## setting function
    x <<- y
    inv <<- NULL
  }
  get <- function() x           ## getting function
  setInverse <- function(solveMatrix) inv <<- solveMatrix       ## set inverse function
  getInverse <- function() inv                                  ## get inverse function
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
