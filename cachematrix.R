## The makeCacheMatrix is a function that creates a special matrix object that caches the inverse of a matrix
## It is actually a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

##  From the example code provided I have updated the matrix inverse object name to 'm' and updated each reference
##  of 'mean' to 'inverse'.

makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL
    ## sets m to NULL to clear previous caches
    ## bellow are the setter and getter for this function allowing the matrix or the inverse to 
    ## be called upon
    set <- function(y) {
      x <<- y 
      m <<- NULL
    }
    get <- function() x 
    ## Function that returns x (the matrix)
    setinverse <- function(inverse) m <<- inverse  
    ## sets stores the inverse
    getinverse <- function() m 
    ## reads out the inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns/computes the inverse of a special matrix created within the
## makeCacheMatrix above and stores it.
## It checks if the inverse has already been computed. 
## If the inverse was computed it will retrieve the result. 
## Otherwise it computes the inverse, and saves it in a cache and returns the result. 

## Again using the example code provided I have changed 'inv' to 'm' and updated each reference of
## 'mean' to 'inverse'
## I have also updated the message to  say 'getting unversed matrix'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inversed matrix")
    return(m)
  }
  ## compute the inverse of the matrix in object 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of x
}
