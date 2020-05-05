
# makeCacheMatrix creates a special 'matrix' object that can cache its inverse
# a list, containing a function to 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix
# calculate the inverse of matrix x with solve(x); matrix x must be invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) 
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# CacheSolve computes the inverse of the special matrix returned by makeChacheMatrix
# if the inverse already has been calculated, it gets the inverse from the cache and skip the computation 
# if the inverse has not been calculated, it sets the value in the cache with setInverse
# when you cache you can save computingpower. yay

cacheSolve <- function(x, ...){
	m <- x$getInverse()
	if(!is.null(m)){ # if the inverse already has been calculated, it should get it from the cache
		message("getting cached data.")
		return(m)
	}
	m <- solve(x$get())
	x$setInverse(m)
	m
}
