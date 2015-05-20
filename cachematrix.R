## The calculation of an inverse matrix can be computationally expensive especially
## in the case of bigger matrices. Many matrix computations use the inverse of the
## matrix. In caching the inverse of a matrix, the computation time is greatly reduced
## The following functions aim to cache an inverse of a matrix

## The following function creates a special "vector" containing a list of functions which
## Sets the value of a matrix, gets the value of a matrix
## Sets the inverse of a matrix, gets the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invmatrix <<- solve
  getinverse <- function() invmatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function calculates the inverse of a matrix, if it does not exist in cache;
## Else it pulls the inverse matrix from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinverse()
  if (!is.null(invmatrix)){
    message("Extracting cached inverse matrix")
    return(invmatrix)
  }
  matrix <- x$get()
  invmatrix <- solve(matrix, ...)
  x$setinverse(invmatrix)
  invmatrix
}
