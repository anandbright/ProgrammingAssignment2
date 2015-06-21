## Put comments here that give an overall description of what your
## functions do

## This function is simply a storage for both matrix and it's transpose. So it has four things two for setting up matrix and its transpose and 
## other two for getting the stored value of matrix and its transpose.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	settranspose <- function(t) m <<- t
	gettranspose <- function() m
	list(set = set, get = get, settranspose = settranspose, gettranspose = gettranspose)
}


## m holds the inverse of matrix a that is supplied. At first it tries to get the value from makeCacheMatrix which contins a list of values.
## in case it returns an empty value this function calculates the transpose and also saves it to the list of makeCacheMatrix for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$gettranspose()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix)
	x$settranspose(m)
	m
}
