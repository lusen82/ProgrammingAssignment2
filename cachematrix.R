## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is used for caching of inverse of matrices.
## It defines four functions; set, get, setsolve and getsolve and
## puts them in a list to be returned.
makeCacheMatrix <- function(x = matrix()) {
  ## The inverse is first reset to NULL when wnating to re-
  ## calculate the matrix inverse for a new matrix or after an update:
  inv <- NULL
  ## THe matrix is reset and the inv variable in the other environment is cleared:
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Returns the matrix
  get <- function() x
  
  ## Sets the inv variable to the solve value:
  setsolve <- function(solve) inv <<- solve
  
  ## Gets the inverted matrix that previous have been solve and put to env:
  getsolve <- function() inv
  
  ##This objct is returned with the functions that can operate on it.
  ## Example: val = makeCacheMatrix(matrix(c(2,0,2,0), 2, 2)), calling
  ## val$get() will return the matrix..
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function returns an inverse of a matrix, if the inverse 
## is created and exists already. If not, the inverse is calculated 
## and is set to the parent environment with the help of setsolve 
## defined in MackeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Try to get the solved inverse and return it if its not null:
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the original matrix:
  data <- x$get()
  
  ## Calculate the inverse of this matrix:
  inv <- solve(data, ...)
  
  ## Store the inverse to the "cached" variable:
  x$setsolve <- inv
  
  ## Return the inversed matrix:
  inv
}
