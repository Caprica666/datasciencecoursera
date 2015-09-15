##
## Caches the inverse of a matrix for faster computation.
##
## Usage:
##  a = matrix(c(1, 0, 0, 0, 2, 0, 0, 0, 3), nrow=3, ncol=3)
##  acache = makeCacheMatrix(a)
##  ainv = cacheSolve(acache)   # computes the inverse of a
##  ainv2 = cacheSolve(acache)  # returns the cached inverse of a
##
##
##
## @function makeCacheMatrix
## Creates a matrix which cache's its inverse once it
## has been solved to avoid recomputation.
## @param x   matrix whose inverse should be cached
##
## @returns a list with the following functions
##    get()           returns the matrix whose inverse we want
##    set(y)          sets the matrix whose inverse we want
##                    (and invalidates the cached inverse)
##    getinverse()    gets the cached inverse (NULL if no inverse)
##    setinverse(inv) sets the cached inverse
##
## To obtain the inverse of the matrix call the cacheSolve
## function - getinverse & setinverse are internal and
## should not be called directly
##
## @see cacheSolve
##
makeCacheMatrix <-function(x = matrix())
{
    m <- NULL
    set <- function(y)
    {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##
## @function cacheSolve
## Works in conjunction with makeCacheMatrix to cache
## a matrix inverse so it will only be computed once.
## @param x   object created by makeCacheMatrix
## @param ... parameters passed to solve() when computing matrix inverse
##
## The matrix inverse is computed and stored in the cacheMatrix (x).
## Subsequent calls to cacheSolve with the same x will use the cached
## inverse. To change the matrix whose inverse is cached, call the
## set function on the cacheMatrix (x) and it will invalidate the
## cached inverse and allow a new one to be computed.
## @see makeCacheMatrix
##
cacheSolve <- function(x, ...)
{
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
