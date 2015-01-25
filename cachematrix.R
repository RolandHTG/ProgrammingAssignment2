## The makeCacheMatrix and cacheSolve functions work together to provide
## a data structure that can store a matrix and its inverse along with
## a caching mechanism to avoid the expensive calculation of the matrix
## inverse once it has already been calculated.


## makeCacheMatrix creates an environment for storing a matrix argument
## and a cache for its inverse. It provides getter and setter functions
## for accessing and modifying those variables.

makeCacheMatrix <- function(thisMatrix = matrix()) {
  thisInverse <- NULL

  setMatrix <- function(newMatrix) {
    thisMatrix <<- newMatrix
    thisInverse <<- NULL
  }
  getMatrix <- function() {
    thisMatrix
  }
  setInverse <- function(newInverse) {
    thisInverse <<- newInverse
  }
  getInverse <- function() {
    thisInverse
  }

  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes a cacheMatrix object, calculates and stores the inverse
## of the matrix back in the object if not already cached otherwise it returns
## the cached inverse.

cacheSolve <- function(cacheMatrix, ...) {

  inverse <- cacheMatrix$getInverse()
  
  if(is.null(inverse)) {
    theMatrix <- cacheMatrix$getMatrix()
    inverse <- solve(theMatrix, ...)
    cacheMatrix$setInverse(inverse)
  } else {
    message("getting cached data")
  }
  
  inverse
}
