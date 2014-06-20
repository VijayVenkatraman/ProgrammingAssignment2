## Functions will be able to cache the inverse of a matrix rather than 
## compute it repeatedly to reduce computation.

## This function creates "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
	## assign NULL to output variable
	invmat <- NULL
	## set the input matrix
	set <- function(y) 
	{
    		x <<- y
    		invmat <<- NULL
    }
	## get the values to display
    get <- function() x
	## calculate the inverse and cache
	setinverse <- function() invmat <<- solve(x)
	## display the function output (inverse matrix)
	getinverse <- function() invmat
	## create special "matrix" object
 	list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## This function computes the inverse of the input matrix or uses the cache obtained ## from special "matrix" object.
cacheSolve <- function(x, ...) 
{
	## Return a matrix that is the inverse of 'x'
	invmat <- x$getinverse()
	## Check whether the output is already available and display
	if(!is.null(invmat)) 
	{
		message("getting cached data")
		return(invmat)
	}
	# Calculate the inverse and display
	data <- x$get()
	invmat <- solve(data)
	x$setinverse()
	invmat
}

