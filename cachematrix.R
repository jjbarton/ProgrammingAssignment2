# This code allows creation of an object that
# can hold a square matrix, and provide a cached
# inverse of that matrix (i.e. once the inverse 
# has been calculated it is cached and the cached
# inverse is returned to the user until the cache
# becomes invalidated).

## makeCacheMatrix
# creates the matrix wrapper object. It returns
# a list with four entries, each of which are functions: 
#   get - gets the matrix
#   set - sets the matrix
#   getinverse - gets the cached inverse of the matrix
#   setinverse - sets the cached inverse of the matrix
#
# Sample usage:
# input.matrix <- matrix(sample(0:9, 9, replace = TRUE), 3, 3)
# cm <- makeCacheMatrix(input.matrix)
# cm$get()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

# This function accepts a matrix wrapper object as parameter.
# If the inverse of the given matrix has already been calculated
# the function returns the cached value. If not, it calculates
# the inverse of the matrix, stores this on the matrix wrapper
# object, and returns the inverse to caller.
# 
# Sample usage:
#
# input.matrix <- matrix(sample(0:9, 9, replace = TRUE), 3, 3)
# cm <- makeCacheMatrix(input.matrix)
# cacheSolve(cm)
# cm$getinverse()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
