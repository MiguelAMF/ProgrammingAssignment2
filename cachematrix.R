# makeCacheMatrix creates a special matrix object that can cache its inverse
# it works as a function containing a list of functions
# it contains the following functions:
# setmatrix: sets the matrix
# getmatrix: returns the matrix
# setinverse: sets the inverse of the matrix. This value is calculeted with cacheSolve
# getinverse: returns the inverse function previously stored by cacheSolve in setinverse
# s is the variable were the inverse matrix will be stored

makeCacheMatrix <- function(x = numeric()) {
  s <- NULL
  setmatrix <- function(y) {
    x <<- y
    s <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve functions finds the inverse matrix of the matrix stored in "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  s <- x$getinverse()       # brings the inverted matrix from "makeCacheMatrix" if exist
  if(is.null(s)) {          # if the inverse matrix is NOT in the cache*
    mat <- x$getmatrix()    # put the matrix into "mat"
    s <- solve(mat)         # compute the inverse matrix 
    x$setinverse(s)         # send the inverse to the cache in "setinverse" of "makeCacheMatrix"
    s
  }
  else{                     # if the inverse matrix IS in the cache
    message("getting cached data")
    s                      # shows the inverse matrix stored in the cache
  }
}

# *I changed this from the example code of "makeVector" in order to avoid negatives that may be confusing

