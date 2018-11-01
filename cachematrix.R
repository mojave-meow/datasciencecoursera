## The following two functions cache a matrix and then solve and
## cache its inverse matrix.

## The function makeCacheMatrix caches a matrix x. The function
## assumes that the matrix is a square matrix (i.e. nrow = ncol).

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The function cacheSolve returns the inverse of a square matrix
## suplied by the function makeCacheMatrix. If the inverse has already
## been cached, the function returns the cached inverse matrix. Otherwise,
## the function will solve and return the inverse matrix.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  a <- x$get()
  s <- solve(a, ...)
  x$setinverse(s)
  s
}
