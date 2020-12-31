## The makeCacheMatrix creates a matrix that allows us to cache the value for
## the inverse of a matrix. The cachesolve function allows us to call the 
## value of the inverse if it exists in the cache, otherwise it calculates it
## and then stores it in cache.

makeCacheMatrix <- function(x = matrix()) {
  
  #sets the value of the inverse to Null
  inverse <- NULL
  
  #creates a variable x in the parent function that is set to the value
  #of the matrix, while resetting the inverse to null
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  #allows us to get the value of the matrix
  get <- function() x
  
  #sets the input value of the function to the inverse
  setinverse <- function(inv) inverse <<- inv
  
  #allows us to get the inverse that is stored
  getinverse <- function() inverse
  
  #returns a list with each of the functions named
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function solves the inverse of the matrix that is held in the object
## created by the function above. If the inverse has been calculated before
## it takes the cached value instead.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()#gets the current value of the inverse in cache
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  ##This runs only if inverse = NULL and caches the inverse before display
  #gets the matrix using the previous function's output
  matrix <- x$get()
  inverse <- solve(matrix,...)#solving for the inverse of the matrix
  x$setinverse(inverse)#sets the inverse in the cache
  inverse
}
