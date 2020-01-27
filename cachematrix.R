## These function create a matrix object that can store a cached 
## copy of its inverse

## Created the CacheMatrix object and defines its methods
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set.inverse <- function(inverse) inv <<- inverse
    get.inverse <- function() inv
    list(set = set,
         get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Calculates the inverse of the CacheMatrix object
## If the inverse has already been calculated and stored - it returns it
## Otherwise the function calculates the inverse, stores it, and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get.inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set.inverse(inv)
    inv
}
