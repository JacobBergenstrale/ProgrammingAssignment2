
## Generates a storable matrix container, that can hold a matrix and 
## its inverse in memory. 

makeCacheMatrix <- function(x = matrix()) {

	inv <<- NULL

	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	
	setInverse <- function(invX) inv <<- invX
	getInverse <- function() inv
	
	list(set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)
}


## Returns the inverse of a cacheMatrix x. If the inverse is already 
## calculated it is retreived from memory, othervise it is 
## calculated. 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
      }
	matrix <- x$get()
	inv <- solve(matrix)
	x$setInverse(inv)
	inv
}
