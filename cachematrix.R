## These two functions are use together to calculate the inverse of a
## matrix and store it in cache for later use. The purpose is to save
## overall computation time by using the cached value of the inverse
## rather than recomputing it whenever needed.

## This function creates and returns a list that contains the original
## matrix and the cached inverted matrix once it has been calculated.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse of the matrix
    setInverse <- function(solve) m <<- solve
    
    # get the value of the inverse of the matrix
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of the original matrix after it
## checks the cache to see if it has already been calculated. If the
## inverse has not yet been calculated, this function will do so and 
## then send it to the cache.

cacheSolve <- function(x, ...) {
    # get the value of the inverse that is currently stored in the cache
    m <- x$getInverse()
    
    # check to see if the inverse has already been calculated;
    # if it hasn't, m will be NULL
    if(!is.null(m)) {   # if true, return the cached inverse
        message("getting cached data")
        return(m)
    }

    # get the value of the matrix
    data <- x$get()
    
    # find the inverse of the matrix
    m <- solve(data, ...)
    
    # set the value of the inverse in the cache
    x$setInverse(m)
    # return a matrix that is the inverse of 'x'
    m
}
