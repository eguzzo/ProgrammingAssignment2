# Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # creates a NULL object
  m <- NULL
  
  # get/set matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  # get/set matrix inverse
  setm <- function(solve) m <<- solve
  getm <- function() m
  
  # return a list of functions for matrix
  list(set = set, get = get, 
       setm = setm,
       getm = getm)
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
casheSolve <- function(x, ...) {
  m <- x$getm()
  
  # return the cached matrix inverse if it's already been calculated
  if(!is.null(m)) {
    return(m)
  }
  
  # calculate the inverse of the matrix
  rol <- x$get()
  m <- solve(rol, ...)
  
  # cache the inverse of the matrix
  x$setm(m)
  
  # return the inverse of the matrix
  m
}
