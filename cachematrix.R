## Put comments here that give an overall description of what your
## functions do

## Function to cache the inverse of matrix to reduce the cost of 
## function finding the inverse of the matrix
## Only Non-singular cases defined

## Write a short comment describing this function
## Utility Function to set, get, setInverse, getInverse of the 
## Matrix as provided by user
makeCacheMatrix <- function(x = matrix()) {
	## inverseMatrix initialized
	inv <-  NULL
	##Takes matrix from user as an argument
	##resets the inverseMatrix value
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	##returns matrix
	get <- function() x
	##Takes matrix as an input to set it as inversematrix
	setInverse <- function(inverseMatrix) inv <<- inverseMatrix
	##returns Inverse Matrix
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse =getInverse)
}


## Write a short comment describing this function

## Checks if the inverseMatrix is present in the 
## environment if yes return it else computes the inverse of the 
## Matix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse() ## Retrives the Inverse Matrix
	## if not NULL then returns InverseMatrix without calculating it
	if(!is.null(inv)) {
		message("getting cached data")
		return inv
	}
	## if matrix present is NULL then retrives the matrix,
	## calculate its inverse and returns it
	newMat <- x$get()
	inv <- solve(newMat)
	x$setInverse(inv)
	inv
}
