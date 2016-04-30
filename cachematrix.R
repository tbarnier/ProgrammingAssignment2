## 
# Programming assignement Week 2 of "R Programming" Coursera MOOC
# 
# Matrix inversion is usually a costly computation and there may be
# some benefit to caching the inverse of a matrix rather than 
# compute it repeatedly 
# The assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#   
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# 
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, we assume that the matrix supplied is always invertible.


##
# makeCacheMatrix constructs a matricx with its inverse caching field initialized with 
# proper values
# Params:
# X: The R native matrix to make "inverse cachable"
# Returns a "Cachable" matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    print(environment(set))
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(inv) m <<- inv
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


