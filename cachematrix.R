## Put comments here that give an overall description of what your
## functions do 

## this function makes a matrix that can cache its inverse by using function makeCacheMatrix()

 makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#trial:matrix <- makeCacheMatrix(matrix(10:14, 2,2)
#matrix$get()

## this function calculates inverse of matrix by using function cacheSolve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#test- getInverse(matrix)
#cacheSolve(matrix)

