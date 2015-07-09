## R function that is able to cache potentially time-consuming computations
## for matrix

## This function creates a special "matrix" object that can cache its inverse. 
## It keeps a list containing a function to
##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the value of the inverse matrix
##	4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'

	i <- x$getinverse()
      if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}

## Test data

## x <- matrix( c(4, 2, 7, 6) , 2, 2)
## i <- makeCacheMatrix(x)

## i$get()
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
 
## cacheSolve(i)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## cacheSolve(i)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

