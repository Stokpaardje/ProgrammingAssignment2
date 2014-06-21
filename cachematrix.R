## Calculate the inverse of a matrix and save the results in a cache for faster retrieval when the inverse is asked multiple times

## makeCacheMatrix creates a special matrix that can save the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
			x <<- y
			i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list (set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Returns the inverse of the matrix. If this is the first time that this method is called, the inverse will be calculated and saved in the matrix. Otherwise, it will return the cached inverse.

cacheSolve <- function(x, ...) {
       
		i <- x$getinverse()
		if(!is.null(i)) {
			return(i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setinverse(i)
		i
}
