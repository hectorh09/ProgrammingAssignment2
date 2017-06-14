## This function efficiently calculates the inverse of a matrix, by only performing calculations
## once and caching results

## create a special matrix which is really a list containing a function to set and get the matrix
## and set and get the value of the inverse

makeCacheMatrix <- function (x=matrix()) {
    m<- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function () x 
    setinverse<- function(inverse) m<<- inverse
    getinverse<- function() m
    list (set = set, get=get, 
        setinverse=setinverse,
        getinverse=getinverse)
}


## this function calculates the inverse of the matrix, but first checks to see if it has been
## solved before, otherwise it solves and caches the value

cacheSolve <- function(x, ...) {
    m<- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m<- solve(data,...)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
        

