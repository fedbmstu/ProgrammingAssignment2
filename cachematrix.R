## Functions makeCacheMatrix and cacheSolve are for cached  
## calculation of inverse matrix

## Function makeCacheMatrix creates special "matrix" object that can cache its inverse  
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get the value of the matrix
    get <- function() x
    #set the value of the inverse matrix
    setinv <- function(inv) inv <<- solve(x)
    #get the value of the inverse matrix
    getinv <- function() inv
    #list containing functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Function calculate inverse of the special "matrix"
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {                    
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
