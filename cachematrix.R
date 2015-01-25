## These functions cache the inverse of a matrix

## makes a list of functions that can set the value of x and m within the parent environment 
## of the following functions (set), return the matrix (get), cache its inverse (setinvmatrix),
## and return the cached inverse.  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## Checks to see if an inverse for matrix x is cached. If so, returns the cached value; if not,
## calculates the inverse matrix, caches that value and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}
