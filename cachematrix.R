## This file defines cache matrix, a special matrix that stores its inverse in an 
## internal cache.

## makeCacheMatrix creates an object representing a cache matrix.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	## This function modifies the content of this matrix.
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	## This function returns the content of this matrix.
	get <- function() x
	
	## This function is used to store the inverse matrix in Cache Variable 'inverse'.
	setInverse <- function(inv) inverse <<- inv
	
	## This function returns the cached inverse matrix.
	getInverse <- function() inverse
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve finds the inverse of the cache matrix given. 
## If the inverse is not in the cache, this function calculates the inverse 
## and stores it in the cache for future use.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	inverse <- x$getInverse()
	
	## Cache is usable.
	if (!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	
	## No cache. Need to compute.
	data <- x$get()
	inverse <- solve(data, ...)
	
	## Save the computed value in the cache.
	x$setInverse(inverse)
	
	inverse
}
