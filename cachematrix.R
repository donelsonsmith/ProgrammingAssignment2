# Donelson Smith
# R_programming_Week 3 project

# These functions are designed to take an input matrix and find and cache the 
# inverse of the matrix using the solve() function

# The first function takes as input a user defined matrix (or matrix object)
# for example:
# mat <- matrix(c(0.1, 2, 0.7, 0.3, 4, 0.1, 0.9, 1.5), nrow = 4, ncol = 4)
# then:
# my_matrix <- makeCacheMatrix(mat)
# cacheSolve(my_matrix)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	set.inv <- function(inverse) i <<- solve(x)
	get.inv <- function() i
	list(set = set, get = get,
             set.inv = set.inv,
             get.inv = get.inv)
}

# The cacheSolve function works together with makeCacheMatrix to return the
# output which is the inverse matrix of the input.

# This will also save that result; if it is called again on the same input,
# it will print a message that it is retrieving cached data

cacheSolve <- function(x, ...) {
	i <- x$get.inv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	matrix <- x$get()
	i <- solve(matrix, ...)
	x$set.inv(i)
	i
}