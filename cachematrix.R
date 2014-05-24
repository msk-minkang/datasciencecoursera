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
## If the inverse has been calculated then it will retrieve from catch.
## i.e. saves computation time
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
