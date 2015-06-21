# Your assignment is to write a pair of functions that cache the inverse of a matrix.

# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.



## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  # set the value of the vector
  set <- function(y) {
    x <<- solve(y)
    m <<- NULL
  }
  
  # get the value of the vector
  get <- function() solve(x)
  
  # set the value of the mean
  setmean <- function(mean) m <<- mean
  
  # get the value of the mean
  getmean <- function() m
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # checks to see if the mean has already been calculated. 
  m <- x$getmean()
  if(!is.null(m)) {
    
    # If so, it gets the mean from the cache and skips the computation. 
    message("getting cached data")
    return(m)
  }
  
  # Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}






makeVector <- function(x = numeric()) {
  m <- NULL
  
  # set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the vector
  get <- function() x
  
  # set the value of the mean
  setmean <- function(mean) m <<- mean
  
  # get the value of the mean
  getmean <- function() m
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}



cachemean <- function(x, ...) {
  
  # checks to see if the mean has already been calculated. 
  m <- x$getmean()
  if(!is.null(m)) {
    
    # If so, it gets the mean from the cache and skips the computation. 
    message("getting cached data")
    return(m)
  }
  
  # Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

