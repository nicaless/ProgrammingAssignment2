## makeCacheMatrix creates a special "matrix object
## that is actually a list of functions to help
## cache the inverse of the matrix

## makeCacheMatrix takes a numeric matrix and creates
## a list of functions to:
## set the inverse of the matrix, get the cached matrix
## set the matrix to a new matrix, 
## and get the cached inverse of the matrix

makeCacheMatrix <- function(x=matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i<<-inverse
  getinverse <- function() i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve computes the inverse of a "special" matrix
## and caches the result
## or retrieves the previous cached result if matrix is unchanged
## assumes matrix is always invertible
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  data <-x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}