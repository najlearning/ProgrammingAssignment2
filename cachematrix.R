## This is an Assignment on "Caching the Inverse of a Matrix"
## The function will create a special "matrix" object that can
## cache its inverse

## The function is named 'makeCacheMatrix'

makeCacheMatrix <- function(x = matrix()) {
  invrse <- NULL
  set <- function(z) {
    x <<- z
    invrse <<- NULL
  }
  
  get <- function() x
  setInvrse <- function(solveMatrix)
    invrse <<- solveMatrix
  getInvrse <- function() invrse
  
  list(set = set, get = get, setInvrse = setInvrse, getInvrse = getInvrse)

}


## The next function will compute the inverse of the special "matrix" from 
## the function makeCacheMatrix.
## The name of the function is 'cacheSolve'

## If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrse <- x$getInvrse()
  if(!is.null(invrse))
  {
    message("Retrieving cached data")
    return(invrse)
  }
  y <- x$get()
  invrse <- solve(y)
  x$setInvrse(invrse)
  invrse
}
