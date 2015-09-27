## Put comments here that give an overall description of what your
## functions do
## the  functions create a special "matrix" object that can cache its inverse. Ideally, the first 
## function create a matrix while the second function compute the inverse of the matrix create in the first function

## Write a short comment describing this function
## this function create a special matrix that containing function to:
## 1/. set the value of the matrix
## 2/. get the value of the matrix
## 3/. set the value of the inverse
## 4/. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
