## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix can generate a list can stores the matrix and has the spot to store its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinver <- function(inver) m <<- inver
        getinver <- function() m
        list(set = set, get = get, 
             setinver = setinver, getinver = getinver)
}

## cacheSolve retrieves the cached inverse if exists, if not then calculate it.

cacheSolve <- function(x, ...) {
        m <- x$getinver()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinver(m)
        return(m)
        ## Return a matrix that is the inverse of 'x'
}