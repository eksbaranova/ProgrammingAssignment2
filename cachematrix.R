makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) { ##set function
            x <<- y
            s <<- NULL
      }
      get <- function() x ##get function
      setsolve <- function(solve) s <<- solve ##set the value of inverse matrix
      getsolve <- function() s ##get the value of inverse matrix
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)##return list of functions
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) { ##if this value (m) in the store than get value from store
            message("getting cached data")
            return(m)
      }
      data <- x$get() ##if this value (m) out of the store than 
      m <- solve(data, ...)##set the value of inverse matrix to m
      x$setsolve(m)
      m 
}
