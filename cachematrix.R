# This function creates a list with 4 functions to:
# set the matrix value
# get the matrix value
# set the inverse value
# get the inverse value
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function returns the matrix inverse
# if the inverse was already computed:
# it does not calculate the inverse and 
# returns the message "getting cached data"
# Otherwise it computes the inverse.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
