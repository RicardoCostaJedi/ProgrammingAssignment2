## For very long vectors, simple operations such as 'mean'
## can consume lots of computing power to be completed
## To prevent this behavior, if your vector is not changing
## it can be cached rather than have it calculated many times


## This code uses a generic R function called 'solve'
## solve() function solves equation a %*% x = b for x, where b is a vector or matrix.
## Usage:
##      solve(a, b, tol, LINPACK = FALSE, ...)
##        a: coefficients of the equation
##        b: vector or matrix of the equation right side
##        tol: the tolerance for detecting linear dependencies in the columns of a
##        LINPACK: logical. Defunct and ignored


## makeCacheMatrix is a function that creates 
## an invertible matrix based on argument 'x'

## ============================================
## IMPORTANT: it assumes 'x' is a square matrix
## ============================================

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the matrix 'm' that is returned as the result of this function 
  m<-NULL
  ## clears the matrix and its inversed that was cached
  set<-function(y){
    x<<-y
    m<<-NULL
}
  ## returns the cached inversed matrix
  get<-function() x
  ## inverts the matrix using 'solve' function
  setmatrix<-function(solve) m<<- solve
  ## saves the inverted matrix in cache
  getmatrix<-function() m
  list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}

## cacheSolve first check is there is an invertible matrix in cache 
## if positive, it will use the cached matrix
## if negative, it will call makeCacheMatrix to create the matrix

cacheSolve <- function(x, ...) {
  ## checks if there is an inverted matrix in cache
  m<-x$getmatrix()
  ## informs it is using the cache, if an inverted matrix already exists
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## gets the matrix 
  matrix<-x$get()
  ## compute its inverse, using 'solve' function
  m<-solve(matrix)
  ## sets the inverse
  x$setmatrix(m)
  ## returns the matrix result
  m
}
