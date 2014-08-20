## The funstions makeCacheMatrix and cacechSolve are designed to illustrate 
## R's scoping rules.
##
## makeCacheMatrix creates a special type of matrix that can cache its 
##     inverse.
##
## cacheSolve determines whether the inverse of a matrix 
##     has already been cached, and returns the cached inverse if 
##     it has been cached. If the inverse has not been cached, cacheSolve
##     generates the inverse and saves it in the cache.

## makeCacheMatrix: create a matrix object that can cache its inverse
## input: a matrix (e.g., a matrix "a" created using 
##	a <- matrix(c(1,5,4,6), nrow = 2, ncol=2)
##	Note: the input is assumed to be an invertible matrix; the
##	code does not check whether it is a matrix or, if it is
##	a matrix, whether it is invertible or not
## return value: a list of functions (set, get, setinverse, getinverse) 

makeCacheMatrix <- function(x = matrix()) {
	## initialize the inverse to null
	m <- NULL
	
	## set (i.e., write) the matrix x
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## get (i.e., retrieve) the matrix x
	get <-function()x
	
	## set the inverse of the matrix 
	setinverse <-function(inverse) m <<- inverse
	
	## get the inverse of the matrix 
	getinverse <-function() m
	
	## return list of four functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve: if an inverse of a matrix isn't already cached, 
##        compute it and place it in a cache using makeCacheMatrix
## input: a special matrix created using makeCacheMatrix
##  	Note: no input type checking is done within cacheSolve; an error will occur
##	if the input is not an object created using makeCacheMatrix
## return value: inverse of the input matrix

cacheSolve <- function(x, ...) {
	## check if the inverse of 'x' is already cached
	m <- x$getinverse()
	
	
	if(!is.null(m)) {
		## inverse was cached, so return it instead of re-computing
		message("getting cached inverse")
		return(m)
	}

	## retrieve the matrix 
	data <- x$get()
	
	## compute inverse of matrix
	m <-solve(data)

	## set the inverse so that it is cached for re-use
	x$setinverse(m)

        ## return the matrix that is the inverse of 'x'
	m
}
