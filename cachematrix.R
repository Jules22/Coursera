##The below function gets a Matrix input, sets and gets the value of it and
## sets and gets the Inverse value of it. The matrix object is able to cache
## its own object.

##taking a Matrix as input

makeCacheMatrix <- function(x = matrix()) {
	invrs <- NULL

##setting the Matrix value

        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x 						##getting the value of the Matrix
        setinvrs <- function(inverse) invrs <<- inverse	##setting the value of the Inverse Matrix
        getinvrs <- function() invrs				##getting the value of the Inverse Matrix
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}


## The next function takes the output of the previous matrix makeCacheMatrix(matrix) as an 
## input and checks whether the inverse matrix has already a value in it or not. 
## In case the inverse matrix has has not been computed, it gets the original matrix data and sets the invertible matrix by using the solve function.
## In case the inverse matrix has some value in it, it returns a message  "Getting cached data" and the cached object.

cacheSolve <- function(x, ...) {

## Getting the value of the matrix from previous function

	invrs <- x$getinvrs()
        if(!is.null(invrs)) {
                message("Getting cached data")
                return(invrs)
        }
## If the value of the matrix is NULL then 

        data <- x$get()
        invrs <- solve(data, ...)
        x$setinvrs(invrs)
        invrs

}
