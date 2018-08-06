## Put comments here that give an overall description of what your
## functions do
##cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {                                              ##set value of matrix
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x                                               ##get value of matrix
  setInv <- function(inverse) invMatrix <<- inverse                       ##set inverse of matrix
  getInv <- function() invMatrix                                          ##get value of inverse of matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInv()                                                         ## get inverse matrix
  if(!is.null(invMatrix)) {                                                       ## if inverse matrix is not null
    message("getting cached data")
    return(invMatrix)                                                       ## return inverse matrix
  }
  data <- x$getMatrix()                                                           ## get original matrix data
  invMatrix <- solve(data, ...)                                                   ## compute its inverse using solve()
  x$setInv(invMatrix)                                                             ## set inverse matrix
  invMatrix                                                                       
}