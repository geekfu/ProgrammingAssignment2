## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## initialize a list of 4 functions with the matrix
## set: set the matrix
## get: get the stored matrix
## setinversion: set the inversion of the matrix
## getinversion: get the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        # initialize the inversion as NULL
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinversion <- function(inversion) i <<- inversion 
        getinversion <- function() i
        
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)

}


## Write a short comment describing this function
## get the inversion of the defaulted invertible matrix using the function solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinversion()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        # solve the matrix
        i <- solve(data, ...)
        x$setinversion(i)
        i
}
