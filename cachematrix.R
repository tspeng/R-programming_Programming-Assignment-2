##Save the cost of computing inverse of a matrix
##using two functions

##makeCacheMatrix function defines a list of four functions
## set the value of the matrix
## get the value of the matrix
## setinverse-set the value of the inverse
## getinverse-get the value of the inverse
makeCacheMatrix <- function(x=matrix()) {
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) iv <<- inverse
    getinverse <- function() iv
    list(set=set,get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##Compute the inverse of a matrix based on whether 
##it has been computed
cacheSolve <- function(x,...) {
    iv <- x$getinverse()
    if (!is.null(iv)) {
        message("getting cached data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setinverse(iv)
    iv
}