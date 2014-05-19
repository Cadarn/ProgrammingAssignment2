## This pair of functions allow us to create a matrix object that caches the inverse of the matrix after it is
## first calculated so that in future calls the cached inverse can be returned instead of recalculating.

## This function creates a "special" version of a matrix which is a really a list containing a function to:
## 1. set the values of the matrix; 2. get the values of the matrix; 3; set the value of the matrix inverse;
## 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y){
              x <<- y
              inv <<- NULL
       }
       get <- function() x
       setinverse <- function(solve) inv <<- solve
       getinverse <- function() inv
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## The function takes an instance of the "special" matrix from makeCacheMatrix() and returns the inverse
## of the matrix first checking to see of the inverse has already been cached and can be returned withour recalculating
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)){
           message("getting cached data")
           return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
