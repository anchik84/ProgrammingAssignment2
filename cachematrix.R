## makeCacheMatrix gets an object of type matrix.
## Then, it extends it so it could cache its inverse matrix value.

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) 
  {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) ix <<- solve
  getsolve <- function() ix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## calculates the inverse value of a matrix x --> if x wasn't yet calculated,
## otherwise returns the cached value

cacheSolve <- function(x, ...) {
  ix<- x$getsolve()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setsolve(ix)
  ix
}
