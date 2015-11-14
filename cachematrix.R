## The first function creates a special "vector", which is really a list containing a function to
## set the value of the matrix, get the value of the matrix,
## set the inverse of the matrix, and get the inverse of the matrix.


## makeCacheMatrix returns a list of functions that the user can call
## that return the inverse of a 2-by-2 matrix
makeCacheMatrix <- function(x = matrix()) {
    ## set c to NULL as a placeholder for a future value
    c <- NULL
    ## setMatrix uses the superassignment operator "<<-" which can modify variables in parent environments
    setMatrix <- function(y) {
        x <<- y
        c <<- NULL
    }
    ## Returns the matrix x
    getMatrix <- function() {
        x
    }
    ## Assigns the variable "solve" to c
    setInvMatrix <- function(solve) {
        c <<- solve
    }
    ## Returns c
    getInvMatrix <- function() {
        c
    }
    ## Returns a list containing all the function defined
    list(set = setMatrix,
         get = getMatrix,
         setInverse = setInvMatrix,
         getInverse = getInvMatrix)
}


## Return a matrix that is the inverse of 'x'
## by using the list function from makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## set c to getInverse() of x
    c <- x$getInverse()
    ## Check to see if c is NULL
    ## if no, return c
    if(!is.null(c)) {
        message("getting cached data")
        return(c)
    }
    ## if c is NULL
    ## return inverse of x
    data <- x$get()
    c <- solve(data, ...)
    x$setInverse(c)
    c
}
