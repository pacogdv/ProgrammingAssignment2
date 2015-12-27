## Put comments here that give an overall description of what your
## functions do

## Create a temp matrix object to solve the inverse of the marix
makeCacheMatrix <- function(x = matrix()) {
	## initialize a null vector
    tempInverse <- NULL
    set <- function(y) {
      x <<- y
      tempInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) tempInverse <<- inverse
    getInverse <- function() tempInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
