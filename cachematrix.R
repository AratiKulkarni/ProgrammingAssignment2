## The follwoing functions compute and cache the inverse of a matrix


## This function creates a metrix object that can hold the inverse of the passed matrix
## It returns a list of functions that are used as input in the following function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inverted_matrix <- NULL
  set <- function(y) {
    ##<<- is used to assign a value to an object from a different environment that the current one
    x <<- y
    inverted_matrix <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inverted_matrix <<- inverse
  getinv <- function() inverted_matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function calculates and/or returns the inverse of a matrix

cacheSolve <- function(x, ...) {
  inverted_matrix <- x$getinv()
  ## Return cached data if available
  if(!is.null(inverted_matrix)) {
    message("getting cached data")
    return(inverted_matrix)
  }
  
  ## Calcualted inverse if its not already cached
  data <- x$get()
  inverted_matrix <- solve(data, ...)
  x$setinv(inverted_matrix)
  inverted_matrix
}
