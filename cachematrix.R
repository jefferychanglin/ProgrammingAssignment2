## Caching the Inverse of a Matrix

## this function This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
      x <<- y
      I <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) 
    {
      I <<- inverse
    }
    getinverse <- function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m   
}

