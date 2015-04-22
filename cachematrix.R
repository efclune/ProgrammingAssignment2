## cachematrix.R
##
## Pair of functions to efficiently cache an inverse matrix calculation the could potentially
## be done repeatedly and be very expensive
## makeCacheMatrix sets up the new structure
## cacheSolve makes the inverse calculation, if it has not been made, keeps it and always
## returns the inverse matrix
## Note: Once makeCacheMatrix has been called, the matrix should be referened only through
## the functions returned from makeCacheMatrix.  Because cacheSolve exists, the only calls
## necessary are foo$set and foo$get (assuming foo stores the function list).  You may wish
## to create wrapper functions to hide these list accesses, but that is not required.


## makeCacheMatrix()
## makeCacheMatrix takes as input a matrix and produces a list of four functions that should
## be used to access the data after that point, both to update the data and to compute
## the inverse.  Changing the input matrix variable after this will not affect the function.

makeCacheMatrix <- function(x = matrix()) 
{
	inv <- NULL		## initialize inverse matrix - initially empty

	set <- function(y)	## function set() stores a new matrix a resets inv to NULL
	{
		x <<-y
		inv <<-NULL
	}

	get<-function() x	## function get() returns the stored matrix

	## function setInverse() caches inverse matrix
	setInverse <- function(inverse) inv <<- inverse

	## function getInverse() returns stored inverse matrix
	getInverse <- function() inv

	## return the list of functions used to access the matrix and inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve()
## cacheSolve takes the CacheMatrix variable created by makeCacheMatrix and returns the
## inverse matrix of the stored matrix value.  If the inverse has already been computed, it
## is returned immediately.  Otherwise, the inverse is computed (using solve()), stored and
## returned

cacheSolve <- function(x, ...) 
{
        ## retrieve cached inverse matrix
	tempinv<-x$getInverse()

	## test whether cache is empty
	if(!is.null(tempinv))
	{
		## if not empty, note retrieval of cached value
		message("getting cached data")
	} else {
		## if cache is empty, grab original matrix, compute and store inverse
		data<-x$get()
		tempinv<-solve(data)
		x$setInverse(tempinv)
	}

	# return inverse matrix, whether from cache of computation
	tempinv
}
