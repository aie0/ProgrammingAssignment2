## A pair of functions that create an inverse cachable object and calculate the inverse of a matrix

## Creates and returns a special matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function() m
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Calculates an inverse of the provided cachable matrix object
cacheSolve <- function(m, ...) {
    i <- m$getInverse();
    
    if(!is.null(i)) {
        message("getting cached data");
        return(i);
    }
    
    data <- m$get();
    i <- solve(data);
    
    m$setInverse(i);
    return (i);
}
