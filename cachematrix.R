## Functions for creating a special object that stores a matrix and and using inverted matrices which caching ability


## makeCacheMatrix function creates cacheable matrix which is a list containing a function to:
##set the value of the matrix,get the value of the matrix,set the value of the inverse, and
##get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i.matrix <- NULL
  
  set <- function(y) {
    x <<- y
    i.matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i.matrix <<- inverse
  getinverse <- function() i.matrix
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## makeCacheMatrix() function computes the inverse of the cacheable matrix returned by makeCacheMatrix
## If the inverse has already been calculated, the cacheSolve() returns the cached inverse

cacheSolve <- function(cacheablematrix, ...) {
  i <- cacheablematrix$getinverse()
  if (!is.null(i)) {
    message("Getting Cached Data")
    return(i)
  }
  data <- cacheablematrix$get()
  i <- solve(data, ...)
  cacheablematrix$setinverse(i)
  i
}
