## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    i <- matrix()
    set <- function(y) {
        x <<- y
        i <<- matrix()
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    
    # return cached data if it exists
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    # if it does not already exist, calculate matrix inverse and store in cache
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
