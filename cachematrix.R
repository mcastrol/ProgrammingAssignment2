## Put comments here that give an overall description of what your
## functions do
##created by marcela castro leon  03/02/2017


## the function makeCacheMatrix 
## returns a list of four elements, 
# which are functions related to x: 
#1º - set the value of the matrix
#2º - get the value of the matrixr
#3ª - set the inverse of the matrix
#4º - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
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


## This function cacheSolve assumes that the x is created with the
## previous function makeCacheMatrix in order to
## call to their functions to obtain the inverse matrix
## saved 
## example of use:
## a) create m1 matrix:
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## b) create an object to use the cache of inverse matrix 
## myMatrix_object <- makeCacheMatrix(m1)
## c) obtain and cache the inverse 
## cacheSolve(myMatrix_object)
##[,1] [,2]
##[1,]    6    8
##[2,]    2    4

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}