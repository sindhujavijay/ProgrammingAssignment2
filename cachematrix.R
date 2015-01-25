## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
	m<-NULL
	set<-function(y)
	{
	x<<-y
	m<<-NULL
	}

	## Get the input functions
	get<-function() x

	## Set the inverse of the matrix
	setinverse<-function(inverse) m<<-inverse

	## Get the inverse of matrix X
	getinverse<-function() m
	list(set=set,get=get,
	setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function
## CacheSolve return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
	## Inverse of X is stored in m
	m<-x$getinverse()

	#Check whether m is not Null
	if(!is.null(m))
	{
		message("getting cached data")
		return(m)
		## Returns inverse matrix from cache data
	}

	## Input natrix x is assigned in data
	data<-x$get()

	## Function 'solve' is used to find inverse of the matrix 'x'
	m<-solve(data,...)
	x$setinverse(m)

	## Returns inverse matrix of 'x'
	m
}
