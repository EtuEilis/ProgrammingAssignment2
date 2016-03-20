## Given a invertible matrix, the following two functions will calculate the inverse matrix 
## or retrieve the inverse matrix from the cache.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The function, makeCacheMatrix creates a special "matrix", which contains 4 functions:
## 1.set function : set the value of the matrix
## 2.get function : get the value of the matrix
## 3.setinverse function : set the value of the inverse of a matrix
## 4.getinverse function : get the value of the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function

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

