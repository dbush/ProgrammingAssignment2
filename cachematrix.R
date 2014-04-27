## The following functions implement caching for potentially costly matrix 
## inversion operations.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. 
## 
## If the inverse has already been calculated (and the matrix has not changed),
## then the function should retrieve the inverse from the cache. When this 
## happens the following message is displayed: "getting cached data".

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if (!is.null(i)) {
    # The inverse has been previously calculated and cached.
    # Show a message and return the value
    message("getting cached data")
    return(i)
  }

  # The inverse has not been previously calculated and cached.
  # Get the data matrix, calculate the inverse and cache it.
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  
  # Return the calculated inverse
  i
}
