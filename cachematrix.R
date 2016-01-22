## makeCacheMatrix creates a special matrix that can cache its inverse

## sets the value of the matrix
## gets the value of the matrix
## sets the inverse of the matrix 
## gets the inverse of the matrix
## This function is used as an input for CacheSolve function

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## (1) cacheSolve function checks if the inverse of the matrix was calculated
## (2) if it was calculated before, 
##     it gets the value from the cache and skips the computation 
## (3) if it was not calculated before,
##     it calculates the inverse of the matrix and sets its value to the
##     cache using setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	  inverse <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        return(inv)
}
