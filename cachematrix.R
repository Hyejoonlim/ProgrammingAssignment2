## R Programming 
## Week 3 Programming Assignment 2: Lexical Scoping

## This function create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function () inv
      list (set = set, get = get, 
            setinverse = setinverse,
            getinverse = getinverse)
}


## Computes the inverse of a square matrix returned by markCacheMatrix
## solve() is used to compute the inverse
## The Matrix input has to be a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
            message ("Getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
