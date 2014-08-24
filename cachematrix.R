## These R functions are able to cache time-consuming inverse 
## of a matrix operation. Taking the inverse of a numeric matrix 
## is an operation that, for a large matrices may take a lot of time, 
## especially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a matrix are not changing, it may make sense 
## to cache the inversed matrix so that when we need it again, 
## it can be looked up in the cache rather than recomputed.

## The first function, makeCacheMatrix creates a 
## special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(matrix = matrix()) {
  inv <- NULL
  set <- function(y) {
    matrix <<- y
    inv <<- NULL
  }
  get <- function() matrix
  setinv <- function(inverseMatrix) inv <<- inverseMatrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

## The following function calculates the inverse of the matrix "matrix"
## created with the above function. However, it first checks to see if 
## the inverse of matrix has already been calculated. 
## If so, it gets the inverse of matrix from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inv in the cache via the setinv function.

cacheSolve <- function(x, ...) {
       
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("calculating inverse of matrix")
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}

