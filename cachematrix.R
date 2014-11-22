##
## Programming Assignment 2: Lexical Scoping
##
## Given a matrix, calculate its inverse.  If the inverse was previously calcu-
## lated and the original matrix has not changed, return the cached inverse 
## matrix.
##
## The first function constructs an object to store the original matrix and its
## inverse.
##
## The second function returns the inverse matrix.  If the inverse matrix was
## already calculated, it will return the cached value.  If the inverse was not 
## previously calculated, it will be calculated with the solve() function and 
## the result will be stored before being returned.
##
## Usage
##
##  ## Create and store a 5x5 matrix
##  mat5x5 <- makeCacheMatrix(matrix(rnorm(25), 5, 5))
##
##  ## Return inverse
##  cacheSolve(mat5x5)
##

makeCacheMatrix <- function(x = matrix()) {
    # Creates a list object that contains a matrix, its inverse and several ac-
    # cessor methods.
    #
    # Args:
    #   x: The matrix to be stored.
    #
    # Returns:
    #   The list object containing the matrix and its inverse.
    #
    
    # Initialize inverse matrix to NULL
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    # Returns a matrix that is the inverse of 'x'.
    #
    # Args:
    #   x: The matrix whose inverse is to be returned, assumed to be invertible.
    #
    # Returns:
    #   The inverse of the matrix 'x'.
    #
    
    # If inverse is already calculated, returned cached value
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # Else get stored matrix, calculate, store and return inverse
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
