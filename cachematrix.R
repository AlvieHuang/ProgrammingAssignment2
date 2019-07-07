############################################################################################
## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##
## Function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated then the cachesolve 
## retrieve the inverse from the cache.
##
## Author: Alvin Huang
## July 7, 2019
############################################################################################


## makeCacheMatrix
# The first function, makeCacheMatrix, creates a special "matrix", which is really a 
# list containing a function to:
  # 1 set the value of the matrix
  # 2 get the value of the matrix
  # 3 set the value of the inverse of the matrix using the solve() function
  # 4 get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inversed) i <<- inversed
  getinverse <- function() i
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve
# This function returns a matrix that is the inverse of 'x'. Function uses a cached matrix if it has been
# calculated before.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


# a <- makeCacheMatrix(matrix(rnorm(25),nrow = 5,ncol = 5))
# a$get()
# a$getinverse()
# cacheSolve(a)
# a$getinverse()  # this is only to show you that the mean has been stored and does not affect anything
# cacheSolve(a)