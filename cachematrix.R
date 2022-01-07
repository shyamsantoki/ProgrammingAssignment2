## Contains functions that calculates inverse of a matrix.
## These function eliminates recomputation of a matrix inverse by using caching.
##
## How to use? -
## 1. Declare a matrix
##    m <- matrix(C(1, 11, 0, 5, 3, 4, 4, 9, 1), nrow = 3, ncol = 3)
## 2. Call makeCacheMatrix, and provide m as an argument. Store return value
##    aMatrix <- makeCacheMatrix(m)
## 3. Call cacheSolve, and provide previously stored return value as an argument.
##    cacheSolve(aMatrix)
##    This function returns inverse of a matrix.
##    If you call this function more than once, you should see a below message:
##    "getting cached data"

## Makes cached matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # Set an inverse to null
    i <- NULL
    
    # Setter function
    # Value of i is resetted whenever value of x is changed
    set <- function(m) {
        x <<- m
        i <<- NULL
    }
    
    # Getter function
    # Value of x, or matrix is returned
    get <- function() {
        x
    }
    
    # Return value of cacheSolve() is set/stored here
    setinverse <- function(inverse) {
        i <<- inverse
    }
    
    # Returns value of an inverse of the matrix
    getinverse <- function() {
        i
    }
    
    # Return value
    # Also parameters/args of cacheSolve() function
    list(get = get, set = set,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Calculates an inverse of a matrix (if needed)

cacheSolve <- function(x, ...) {
    
    # Gets an inverse stored in makeCacheMatrix,
    # accessed through parameter x
    i <- x$getinverse()
    
    # If value is not null, it is returned
    if (!is.null(i)) {
        message("getting cached data")
        i
    }
    
    # Else it is calculated
    else {
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
    }
    
}
