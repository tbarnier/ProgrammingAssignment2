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


##
# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setsolve`
# function.
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
mat<-matrix(c(1,2,3,4),2,2)
x<-makeCacheMatrix(mat)
cacheSolve(x)
cacheSolve(x)

makeCacheMatrix <- function(x = matrix()) {
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
}
