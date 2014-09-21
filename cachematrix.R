## The functions cache the inverse of a matrix.

## The function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL 
	}
	get <- function () x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, 
	setinv = setinv, getinv = getinv)

}


## The function calculates the inverse of a matrix and returns it. If the inverse has been calculated, the function gets it from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv) 
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv 
}
