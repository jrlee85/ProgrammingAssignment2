## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Similarly to makeVector(), makeCacheMatrix begins by initialising two objects
## x and i, with i initially set to null. The function then defines the setters
## and getters for x and for i.
## makeCacheMatrix returns a list containing these setters and getters.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve checks if there is already a cached i (matrix inverse) for the
## given vector x. If there is, it returns this i without performing any further
## calculations. If not, the inverse is calculated using the solve() function
## and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
