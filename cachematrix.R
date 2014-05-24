## These functions are used to cache inverse of a matrix, which is usually a costly computation.
## By caching the inverse of a matrix rather than computing it repeatedly, it can significantly reduce the computation time.

## Creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}
 

## Computes the invesre of the special matrix returned by makeCacheMatrix.
## If the inverse has not been calculated then, cahcheSolve() will calculate the inverst of the matrix.
## If the inverse has been calculated then it will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)
	inv
}
