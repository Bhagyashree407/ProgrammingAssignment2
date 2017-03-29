#makeCacheMatrix() function creates special matrix that can catche its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  Cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(Inverse){
    Cache <<- Inverse
  }
  getInverse <- function(){
    Cache
  } 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



#cacheSolve() function computes the inverse of the matrix returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
#the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse))  {
    message("getting cached data")
    return(inverse)
  }
  
  inverse <- solve(x$get())
  return(inverse)
}


#test1:
#Matrix <- makeCacheMatrix(matrix(1:4,2,2,byrow = TRUE))
#Matrix$get()
#Matrix$getInverse()
#cacheSolve(Matrix)


#test2:
#Matrix1 <- makeCacheMatrix(matrix(c(11,21,31,41),2,2))
#Matrix1$get()
#Matrix1$set(matrix(c(1,2,3,4),2,2))
#Matrix1$get()
#Matrix1$getInverse()
#cacheSolve(Matrix1)