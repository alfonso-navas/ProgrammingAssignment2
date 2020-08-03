## These functions create an special ¨matrix¨ object that stores a numeric 
## matrix and cache its inverse.

## The following function creates a special "matrix" object, which is really a 
## list of functions to: 1. Set the matrix; 2. Get the matrix; 3. Set the 
## inverse of the matrix; 4. Get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## The following function computes and returns the inverse of a special "matrix" 
## object created with the above function. However, if the inverse has already 
## been calculated (and the matrix has not changed), this function will retrieve 
## the inverse from the cache, computation the calculation.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setinverse(inv)
        inv
}