## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. These functions provide a way to cache the inverse of a matrix.
## Note that the matrix supplied is assumed to be invertible!


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
	inverse <- NULL
	
	set <- function(y) {
		matrix <<- y
		inverse <<- NULL
	}
	
	get <- function() matrix
	
	setInverse <- function(newInverse) inverse <<- newInverse
	
	getInverse <- function() inverse
	
	list(
		set = set, 
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
	
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(cacheMatrix, ...) {
	inverse <- cacheMatrix$getInverse()
	if(!is.null(inverse)) {
		return(inverse)
	}
	inverse <- solve(cacheMatrix$get(), ...)
	cacheMatrix$setInverse(inverse)
	cacheMatrix
}