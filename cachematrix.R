## The functions below are used to create a special object that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing 
## a function to set the value of the matrix, get the value of the matrix, set the value of the 
## inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function creates the inverse of the special "matrix" created with the above 
## function. However, it first checks to see if the inverse has already been created. 
## If so, it gets the inverse from the cache and skips the computation required to create the inverse. 
## Otherwise, it creates the inverse of the matrix sets the matrix inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i 
}
