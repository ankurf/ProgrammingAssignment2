#These functions will calculate the inverse of a matrix or retrieve it from cache.

#Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.
#(1)set the value of the matrix.
#(2)get the value of the matrix.
#(3)setinverse sets the value of the inverse of the matrix.
#(4)getinverse gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#This function cacheSolve calculates the inverse of the special “matrix” created by makeCacheMatrix function. 
#If the inverse has been calculated, then the cacheSolve will retrieve the inverse from the above function 
#otherwise it gets the matrix stored and calculates the inverse, and x$setinverse(m) stores it in the 
#object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
