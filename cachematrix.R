## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix prepares a special matrix that can cache it's inverse
## cacheSolve function uses special caching to return the solution if found.

## Write a short comment describing this function
## solved inverse is cached in 'inv' variable
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## solves inverse of x, 
## if inverse of x is found in cache returns from cache
## else calculates inverse
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
