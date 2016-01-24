## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ## Set the value of the matrix.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get the value of the matrix.
  get <- function() x
  
  ## Calculate the inverse matrix.
  setInverse <- function(solve) m <<- solve
  
  ## Get the inverse matrix.
  getInverse <- function() m
  list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Computes the inverse of the special "matrix".
cacheSolve <- function(x, ...) {
  
  ## Checks to see if the inverse has already been calculated.
  m <- x$getInverse()
  if(!is.null(m)) {
    
    ## If it has it gets the inverse from the cache.
    message("getting cached data")
    return(m)
  }
  
  ## Else, it calcualtes the inverse and sets this value to the cache.
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
