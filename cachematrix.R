## The makeCacheMatrix function gets and saves the user-inputted matrix
## in R's cache.
## Then, the cacheSolve function takes the makeCacheMatrix as the input
## to output the inverse of the matrix that the user previously inputted.


## The makeCacheMatrix function takes a matrix as the input.  It gets the
## matrix that the user inputted and then sets the matrix so it is saved
## in R's cache, which is different from the current environment

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The cacheSolve function takes the makeCacheMatrix function, which 
## includes the user-inputted matrix, as the input.  It uses the local
## functions of the makeCacheMatrix function to retrieve the matrix
## and then calculates the inverse of the matrix using the solve()
## function.  Afterwards, the inverse of the matrix is shown.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
	## Return a matrix that is the inverse of 'x'
}