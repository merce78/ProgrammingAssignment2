## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   mi <- NULL
   set <- function(y){
     x <<- y
     mi <<- NULL
   }
   get <- function() x
   setInverse <- function(minverse) mi <<- minverse
   getInverse <- function() mi
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getInverse()
  if(!is.null(mi)){
    message("getting cached data")
    return(mi)
  }
  xr<- x$get
  mi <- solve(xr)
  x$setInverse(mi)
  mi
}
