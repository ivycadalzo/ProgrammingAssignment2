## This code contains two functions that cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set=set, get=get,
         setinv=setinv,
         getinv=getinv
         )
}


## The cacheSolve function computes the inverse of the special "matrix" returned by the makeCacheMatrix above.
## If the inverse has already been calculated, then the function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
