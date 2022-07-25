## The makeCacheMatrix is a function that creates a special matrix object that caches the inverse of a matrix
##  From the example code provided I have updated the matrix inverse object name to 'm' and updated each reference
##  of 'mean' to 'inverse'

makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL
    set <- function(y) {
      x <<- y 
      m <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) m <<- inverse  
    getinverse <- function() m 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Again using the example code provided I have changed 'inv' to 'm' and updated each reference of
## 'mean' to 'inverse'
## I have also updated the message to  say 'getting inversed matrix'

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inversed matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
