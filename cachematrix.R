# The 'makeCacheMatrix' is a function that will create and return a list of functions
# The list of functions (respectively) store the value of the matrix, get the value of a matrix
# store the cached value of the matrix's inverse, and get said value.

makeCacheMatrix <- function(x = numeric()){
    # The above makes 'makeCacheMatrix' a function that takes in a variable 'x',
    # which it creates/coerces to be numeric. This exercise assumes 'x' is
    # already a square matrix of numbers, so we should be set.
    
    i <- NULL
    # This sets the initial value of the inverse (variable 'i') to NULL,
    # an invalid number. This is because nothing should be cached to start out with.
    
    setMatrix <- function(y){
        x <<- y
        i <<- NULL
    }
    # This is a function that allows you to assign a new matrix of values ('y') to 'x', if you so desire.
    # If there was an existing cached value for 'i', it is removed, because there is a new matrix so it is no longer valid.
    # The '<<-' operator takes these new values assigned within the new function
    # and moves them up to the parent level of the original function.
    
    
    getMatrix <- function() {
        x
    }
    # This function will give you the stored matrix.
    
    setInverse <- function(inverse) {
        i <<- inverse
    }
    # This function will cache the 'inverse' argument (used in the cacheSolve function)
    # as 'i'.
    
    getInverse <- function() {
        i
    }
    # This gets you the cached value of 'i'.
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}
# This creates a list of functions, as described earlier.

cacheSolve <- function(x, ...){
    # Creates a function named 'cacheSolve'. This will (hopefully) take in the list you
    # created in the previous function, which you should have assigned to a name (given as 'x' here).
    
    i <- x$getInverse()
    # Obtains the cached value, either NULL or a valid value.
    if(!is.null(i)){
        message("getting cached inverse")
        return(i)
    }
    # If the cached value is not NULL, and exists, it is returned.
    
    data <- x$getMatrix()
    i <- solve(data, ...)
    x$setInverse(i)
    # If the cached value is NULL, it hasn't been calculated. The above
    # gets the matrix and calculates the value for the inverse. Then this value
    # is cached; set to 'i'.
    
    i
    # Returns the value of the inverse.
}