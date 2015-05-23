## makeCacheMatrix() defines sub-functions to initialize, set and retrieve variables
## intended to store a matrix and its inverse.  cacheSolve() is intended to be used
## with makeCacheMatrix(); it returns the matrix inverse if already calculated, and
## calculates and returns the matrix inverse if not already calculated.

## makeCacheMatrix() defines functions to initialize, set, and retrieve variables
## intended to store a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(matr) m <<- matr
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
  
}


## When passed the makeCacheMatrix() function that itself has an invertible
## square matrix as an argument, cacheSolve() checks whether the variable 'm'
## has already been set, and if not, calls makeCacheMatrix() to populate it
## with the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
