## Below are two functions that are used to create a special object
## that stores a square matrices and cache's its Inverse.

## This function, makeCacheMatrix creates a special "matrix" object
## This object is really a list containing a function to
##setMatrix : the value of the matrix
##getMatrix : get  the value of the matrix
##setInv: set the value of the Inverse
##getInv: get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  
  I<- NULL
  setMatrix <- function(y) {
    x <<- y
    I <<- NULL
  }
  getMatrix <- function() x
  setInv <- function(Inv) I <<- Inv
  getInv <- function() I
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInv = setInv,
       getInv = getInv)
}


##This function, cacheSolve computes the Inverse of the special "matrix"
##returned by makeCacheMatrix above.
##If the Inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieves the Inverse from the cache.

cacheSolve <- function(x, ...) {
  I <- x$getInv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$getMatrix()
  I <- solve(data, ...)
  x$setInv(I)
  I
}

#Test Code:
squareMatrix <- matrix(runif(16,1,10),4,4)
ICacheObj <- makeCacheMatrix(squareMatrix)


cacheSolve(ICacheObj)
cacheSolve(ICacheObj)
cacheSolve(ICacheObj )

