## Pair of functions that cache the inverse of a matrix
## Usage: Pass the result of a makeCacheMatrix call to cacheSolve 

#' Util function that set the matrix and the inverse in an environment


makeCacheMatrix <- function(x = matrix()) {
  # todo error if x is not a matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        inv<- x$getinverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        mat<-x$get()
        inv<- solve(mat, ...)
        x$setinverse(inv)
        inv
}
