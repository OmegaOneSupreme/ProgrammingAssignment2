# Matrix inversion, is sometimes an internsive computation depending on the size of the matrix.
# There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The following function creates a special matrix object and caches its inverse followed by another
# function checks to see if the given matrix's inverse is already calculated and if not, calculates 
# for the given matrix's inverse.


## makeCacheMatrix caches the inverse of a special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
    
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setInverse <- function(inver) inver <<- inverse
    getInverse <- function() inver
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}




## Calculates the inverse of the special "matrix" object created with makeCacheMatrix. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets 
# the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
# of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inver <- x$getInverse()
    if (!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    mat <- x$get()
    inver <- solve(mat, ...)
    x$setInverse(inver)
    inver
    
}


