## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. This functions create a special "matrix" object that can cache its inverse, so
## the operation can be executed with a lower cost

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  setmatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  getmatrix <- function() x
  setinversematrix<- function(im) inverseMatrix <<-im
  getinversematrix <- function() inverseMatrix
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

  
  ##test if inversematrix is cached
  inversematrix <- x$getinversematrix()
  if (!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  } else {

    ##if inverse matrix is not cached, execute the operation
    inversematrix  <- solve(x$getmatrix(), ...)
    x$setinversematrix(inversematrix)
    inversematrix
  }
}




