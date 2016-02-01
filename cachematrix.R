## Calculates the inverse of a matrix once and puts it in cache
## for future use.

## Useful if the matrix is very large
## because calculating the inverse is time consuming. 


## makeCacheMatrix takes a matrix as input and returns a list that 
## contains functions to set and get the value of the matrix, 
## and then set and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix using the list created
## by makeCacheMatrix.

## If the inverse has already been calculated, gets the 
## inverse from the cache instead of calculating again.

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
