
## The following two functions can be employed to calculate the inverse of a
## matrix and cache the result. This can be used when the resulting inverse
## is required for several operations within a program and its computation
## takes a long time. In this case, the cached inverse can be loaded and used 
## for further operations.

## This function builds the framework for caching the inverse of a matrix. 
## It creates a set of functions and two data objects, which it 
## returns to the parent environment in the form of a list (later to be used 
## by the cacheSolve() function). 
## The default for the input argument is set to be an empty matrix.

makeCacheMatrix <- function(x = matrix()) {
      
      # m is created
      m <- NULL
      # set function is defined. The argument y is assigned to the matrix x
      # in the parent environment (makeCacheMatrix()). Object m in the parent 
      # environment is set to NULL, since there exists no cached value for the 
      # inverse if the matrix was just changed. Therefore, an incorrect inverse
      # cannot be retrieved by accident.
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      # The get() function retrieves x from the parent environment of 
      # makeCacheMatrix().
      get <- function() x
      
      # This function takes the inverse of the matrix and makes it accessible in 
      # the parent environment of makeCacheMatrix().
      setinverse <- function(solve) m <<- solve
      
      # This functions returns the cached inverse.
      getinverse <- function() m
      
      # Here, the return list is created: each element is named, which allows 
      # the use of the $ operator.
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## This function calculates the inverse of the matrix given as an argument.
## It first checks in the cache if the calculation has already been done.
## If so, the result is taken from there.
## If not, the inverse is calculated, cached and returned.

cacheSolve <- function(x, ...) {
      
      # Attempt to retrieve the inverse from the object x which was passed as 
      # an argument to cacheSolve().
      m <- x$getinverse()
      
      # Check whether the inverse has already been calculated.
      # If the value is not NULL (NULL --> new input matrix), then there exists 
      # a valid cached inverse, which can be returned to the parent environment.
      if(!is.null(m)) {
            # print message
            message("getting cached data")
            # return of the inverse of the matrix
            return(m)
      }
      
      # If the inverse has not been calculated before:
      # Store the values of the matrix in the local object "data".
      data <- x$get()
      
      # Calculate the inverse of the local object and store it into m.
      m <- solve(data, ...)
      
      # Cache the inverse for later use.
      # When calling cacheSolve() again, m is not NULL anymore. 
      x$setinverse(m)
      
      # Return the inverse.
      m
}
