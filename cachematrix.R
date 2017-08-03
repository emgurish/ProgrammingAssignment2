## Below find two functions used for caching the inverse of a matrix

## The first function makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        in <- NULL
        set <- function(z){
            x <<- z
            in <<- NULL
        }
        get <- function() x
        setIn <- function(inverse) in <<- inverse
        getIn <- function() in
        list(set = set, get = get, setIn = setIn, getIn = getIn)
}


## The second function cacheSolve computes the inverse of the special matrix that is returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        in <- x$getIn()
        if(!is.null(in)) {
            message("getting cached matrix")
            return(in)
        }
        xmatrix <- x$get()
        in <- solve(xmatrix, ...)
        x$setIn(in)
        in
}
