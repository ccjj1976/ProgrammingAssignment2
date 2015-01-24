## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv_matrix <-NULL
  
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  getori_matrix <-function() x
  
  setinverse <-function(inv) inv_matrix <<-inv
  getinverse <-function() inv_matrix

  list(set=set, getori_matrix = getori_matrix,
       setinverse = setinverse,
       getinverse = getinverse)

## Return a list to function "cacheSolve".
}


## This function (cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix has not changed, then the cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$getori_matrix()
  inv_matrix <- solve(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix 

## Return a matrix (inv_matrix) that is the inverse of 'x'.
}
