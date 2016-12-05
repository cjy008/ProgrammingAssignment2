## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse.
# set function: sets the y parameter as the x matrix in the function
# get function returns the x variable inside of the function
# setinverse: similar to the set function as it sets the inverse inside the
#           function
# getinverse: similar to the get function as it gets the inverse inside the
#           function
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

# This function calculates the inverse of the special "matrix" returned by the
# makeCacheMatrix function.
# If the inverse has already been calculated, then it will retrieve it from the 
# cache. Else, it inverse will be calculated. The data stores the matrix stored
# within makeCacheMatrix, then m calculates the inverse and x$setinverse(m) stores
# the inverse into the m object back into the makeCacheMatrix.
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
